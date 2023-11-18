
//@HEADER
// ***************************************************
//
// HPCG: High Performance Conjugate Gradient Benchmark
//
// Contact:
// Michael A. Heroux ( maherou@sandia.gov)
// Jack Dongarra     (dongarra@eecs.utk.edu)
// Piotr Luszczek    (luszczek@eecs.utk.edu)
//
// ***************************************************
//@HEADER

/*!
 @file SparseMatrix.hpp

 HPCG data structures for the sparse matrix
 */

#ifndef HPCGMATRIX_HPP
#define HPCGMATRIX_HPP

#include <cassert>
#include <cstdlib>
#include <cmath>
#include <vector>
#include "DoubleVector.hpp"
extern "C" {
  #include "mmio.c"
}

struct HPCGMatrix_STRUCT {
  int numberOfRows; //!< number of rows
  int numberOfColumns;  //!< number of columns
  int numberOfNonzeros;  //!< number of nonzeros
  //array with the number of nonzeros in row i at position i
  int * nonzerosInRow;
  //matrix with the column index of the j-th element of row i at position (i,j)
  int ** mtxInd; //!< matrix indices
  //matrix with the value of the j-th element of row i at position (i,j)
  double ** matrixValues; //!< values of matrix entries
  //matrix with a reference to the diagonal value of row i at position i
  double ** matrixDiagonal; //!< values of matrix diagonal entries
};
typedef struct HPCGMatrix_STRUCT HPCGMatrix;

inline void MakeSymmetricHPCG(HPCGMatrix & A);

inline double InitializeHPCGMatrix(HPCGMatrix & A, const char* filename) {
  FILE * file;
  MM_typecode matcode;
  int lastInd = 0;
  int lastRow = 0;
  int currNumNon0 = 0;
  double lastValue;
  int curInd;
  double maxValue = 0;
  bool symmetric = false;

  //parse first two lines
  file = fopen(filename, "r");

  mm_read_banner(file, &matcode);
  if(((char*) matcode)[3] == 'S') {
    symmetric = true;
  }
  mm_read_mtx_crd_size(file, &A.numberOfRows, &A.numberOfColumns, &A.numberOfNonzeros);

  //allocate structures that do not depend on the maximum number of nonzeros
  A.nonzerosInRow = (int*) malloc(A.numberOfRows*sizeof(int));
  A.mtxInd = (int**) malloc(A.numberOfRows*sizeof(int*));
  A.matrixValues = (double**) malloc(A.numberOfRows*sizeof(double*));
  A.matrixDiagonal = (double**) malloc(A.numberOfRows*sizeof(double*));
  for(int i=0; i<A.numberOfRows; i++) {
    A.matrixDiagonal[i] = (double*) malloc(sizeof(double));
  }

  //find the largest number of nonzeros per line
  int* cur_num_nonzeros = (int*) malloc(A.numberOfRows*sizeof(int));
  for(int i=0; i<A.numberOfRows; i++) {
    cur_num_nonzeros[i] = 0;
  }

  while (true) {
    if (fscanf(file, "%d", &lastRow) == -1) {
      break;
    }
    cur_num_nonzeros[lastRow-1]++;
    fscanf(file, "%d", &lastInd);
    fscanf(file, "%lf", &lastValue);
  }
  int max_num_nonzeros = 0;
  for(int i=0; i<A.numberOfRows; i++) {
    if (max_num_nonzeros < cur_num_nonzeros[i]) {
      max_num_nonzeros = cur_num_nonzeros[i];
    }
    A.nonzerosInRow[i] = cur_num_nonzeros[i];
  }
  fclose(file);
  file = fopen(filename, "r");
  
  //allocate remaining structures
  for(int i=0; i<A.numberOfRows; i++) {
    A.mtxInd[i] = (int*) malloc(max_num_nonzeros*sizeof(int));
  }
  for(int i=0; i<A.numberOfRows; i++) {
    A.matrixValues[i] = (double*) malloc(max_num_nonzeros*sizeof(double));
  }

  //skip the first two lines of the input
  mm_read_banner(file, &matcode);
  mm_read_mtx_crd_size(file, &A.numberOfRows, &A.numberOfColumns, &A.numberOfNonzeros);

  //reinitialize the number of nonzeros per line
  for(int i=0; i<A.numberOfRows; i++) {
    cur_num_nonzeros[i] = 0;
  }

  //parse the rest of the matrix
  while (true) {
    if (fscanf(file, "%d", &lastRow) == -1) {
      break;
    }
    lastRow--;
    cur_num_nonzeros[lastRow]++;
    fscanf(file, "%d", &lastInd);
    curInd = cur_num_nonzeros[lastRow]-2;
    while(curInd >= 0 && A.mtxInd[lastRow][curInd] > lastInd-1) {
      A.mtxInd[lastRow][curInd+1] = A.mtxInd[lastRow][curInd];
      A.matrixValues[lastRow][curInd+1] = A.matrixValues[lastRow][curInd];
      curInd--;
    }
    A.mtxInd[lastRow][curInd+1] = lastInd-1;
    fscanf(file, "%lf", &lastValue);
    A.matrixValues[lastRow][curInd+1] = lastValue;
    if(lastValue > maxValue) {
      maxValue = lastValue;
    } else if(lastValue < -maxValue) {
      maxValue = -lastValue;
    }
    if (lastInd-1 == lastRow) {
      A.matrixDiagonal[lastRow][0] = lastValue;
    }
  }

  if(symmetric) {
    MakeSymmetricHPCG(A);
  }

  return maxValue;
}

inline void PrintHPCGMatrix(HPCGMatrix & A) {
  for(int i=0; i<A.numberOfRows; i++) {
    for(int j=0; j<A.nonzerosInRow[i]; j++) {
      printf("%d:%d:%lf ", i, A.mtxInd[i][j], A.matrixValues[i][j]);
    }
    printf("\n");
  }
  printf("\n");
}

inline void DeleteHPCGMatrix(HPCGMatrix & A) {
  free(A.nonzerosInRow);
  for(int i=0; i<A.numberOfRows; i++) {
    free(A.matrixDiagonal[i]);
    free(A.matrixValues[i]);
    free(A.mtxInd[i]);
  }
  free(A.matrixDiagonal);
  free(A.matrixValues);
  free(A.mtxInd);
  return;
}

inline void CopyHPCGMatrix(HPCGMatrix & original, HPCGMatrix & copy) {
  //allocate space
  copy.nonzerosInRow = (int*) malloc(original.numberOfRows*sizeof(int));
  copy.matrixDiagonal = (double**) malloc(original.numberOfRows*sizeof(double*));
  copy.mtxInd = (int**) malloc(original.numberOfRows*sizeof(int*));
  copy.matrixValues = (double**) malloc(original.numberOfRows*sizeof(double*));
  for(int i=0; i<original.numberOfRows; i++) {
    copy.mtxInd[i] = (int*) malloc(original.nonzerosInRow[i]*sizeof(int));
    copy.matrixValues[i] = (double*) malloc(original.nonzerosInRow[i]*sizeof(double));
    copy.matrixDiagonal[i] = (double*) malloc(1*sizeof(double));
  }

  //copy
  copy.numberOfRows = original.numberOfRows;
  copy.numberOfColumns = original.numberOfColumns;
  copy.numberOfNonzeros = original.numberOfNonzeros;

  for(int i=0; i<original.numberOfRows; i++) {
    copy.nonzerosInRow[i] = original.nonzerosInRow[i];
    copy.matrixDiagonal[i][0] = original.matrixDiagonal[i][0];
    for(int j=0; j<original.nonzerosInRow[i]; j++) {
      copy.mtxInd[i][j] = original.mtxInd[i][j];
      copy.matrixValues[i][j] = original.matrixValues[i][j];
    }
  }
}

//assumes A is a lower triangular matrix and copies its element above the diagonal
inline void MakeSymmetricHPCG(HPCGMatrix & A) {
  //calculates the real number of nonzeros
  std::vector<int> nonzerosInRow(A.numberOfRows);
  for(int i=0; i<A.numberOfRows; i++) {
    nonzerosInRow[i] = A.nonzerosInRow[i] - 1;
    for(int j=0; j<A.nonzerosInRow[i]; j++) {
      nonzerosInRow[A.mtxInd[i][j]]++;
    }
  }
  //stores a copy of the matrix and reallocates memory
  HPCGMatrix B;
  CopyHPCGMatrix(A, B);
  for(int i=0; i<A.numberOfRows; i++) {
    free(A.matrixValues[i]);
    free(A.mtxInd[i]);
    A.matrixValues[i] = (double*) malloc(nonzerosInRow[i]*sizeof(double));
    A.mtxInd[i] = (int*) malloc(nonzerosInRow[i]*sizeof(int));
    A.nonzerosInRow[i] = nonzerosInRow[i];
  }
  A.numberOfNonzeros = B.numberOfNonzeros*2-A.numberOfRows;
  //copies the original nonzeros in A
  std::vector<int> currNonzerosRow(A.numberOfRows, 0);
  for(int i=0; i<B.numberOfRows; i++) {
    for(int j=0; j<B.nonzerosInRow[i]; j++) {
      A.mtxInd[i][currNonzerosRow[i]] = B.mtxInd[i][j];
      A.matrixValues[i][currNonzerosRow[i]] = B.matrixValues[i][j];
      currNonzerosRow[i]++;
    }
  }
  //adds the transposed nonzeros to A
  for(int i=0; i<B.numberOfRows; i++) {
    for(int j=0; j<B.nonzerosInRow[i]; j++) {
      if(B.mtxInd[i][j] != i) {
        A.mtxInd[B.mtxInd[i][j]][currNonzerosRow[B.mtxInd[i][j]]] = i;
        A.matrixValues[B.mtxInd[i][j]][currNonzerosRow[B.mtxInd[i][j]]] = B.matrixValues[i][j];
        currNonzerosRow[B.mtxInd[i][j]]++;
      }
    }
  }
}

//computes A*v and stores the result in res
void SpMV(HPCGMatrix & A, DoubleVector & v, DoubleVector & res) {
  for(int i=0; i<res.length; i++) {
    res.values[i] = 0;
  }
  for(int i=0; i<res.length; i++) {
    for(int j=0; j<A.nonzerosInRow[i]; j++) {
      res.values[i] += A.matrixValues[i][j]*(v.values[A.mtxInd[i][j]]);
    }
  }
}

#endif // SPARSEMATRIX_HPP
