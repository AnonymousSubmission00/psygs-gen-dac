#include <cassert>
#include <omp.h>
#include "../include/local_gs_data.hpp"

/*
//computes one step of SYMGS using OMP and multi-coloring/dependency graph
void ComputeSYMGS_GC(ContinousLocalGSData* localData, DoubleVector x, int num_colors, int num_threads) {
  #pragma omp parallel firstprivate(localData, x, num_colors, num_threads)
  {  
  	double* xv = x.values;
  	int size = x.length;
  	
    for (int color=0; color<num_colors; color++) {
    
      #pragma omp for schedule(static, 1)
      for(int thread=0; thread<num_threads; thread++) {
      	ContinousLocalGSData threadData = localData[thread];
      	int numRows = threadData.numRows[color];
      	 	
      	int* colorRows = threadData.rows_idx[color];
      	int* colorNumberOfNonzeros = threadData.nonzerosInRow_idx[color];
      	double* colorRhs = threadData.rhs_idx[color];
      	double** colorValues = threadData.matrixValues_idx_idx[color];
      	int** colorIdx = threadData.mtxInd_idx_idx[color];
      	double** colorDiagonal = threadData.matrixDiagonal_idx[color];
      	
      	for (int row_index=0; row_index<numRows; row_index++) { 
      	  int row = colorRows[row_index]; 
      	  int currentNumberOfNonzeros = colorNumberOfNonzeros[row_index];
      	  double sum = colorRhs[row_index];
      	  double* currentValues = colorValues[row_index];
      	  int* currentIdx = colorIdx[row_index];
      	  double* currentDiagonalIdx = colorDiagonal[row_index];

		  for (int j=0; j<currentNumberOfNonzeros; j++) {
		    int curCol = currentIdx[j];
		    sum -= currentValues[j] * xv[curCol];
		  }
		  double currentDiagonal = *currentDiagonalIdx;
		  
		  sum += xv[row]*currentDiagonal; // Remove diagonal contribution from previous loop

		  xv[row] = sum/currentDiagonal;
        }
      }
    }

    // Now the back sweep.
    for(int color=num_colors-1; color>=0; color--) {
    
      #pragma omp for schedule(static, 1)
      for(int thread=0; thread<num_threads; thread++) {
      	ContinousLocalGSData threadData = localData[thread];
      	int numRows = threadData.numRows[color];
      	
      	int* colorRows = threadData.rows_idx[color];
      	int* colorNumberOfNonzeros = threadData.nonzerosInRow_idx[color];
      	double* colorRhs = threadData.rhs_idx[color];
      	double** colorValues = threadData.matrixValues_idx_idx[color];
      	int** colorIdx = threadData.mtxInd_idx_idx[color];
      	double** colorDiagonal = threadData.matrixDiagonal_idx[color];
      	
      	for (int row_index=numRows-1; row_index>=0; row_index--) { 
      	  int row = colorRows[row_index]; 
      	  int currentNumberOfNonzeros = colorNumberOfNonzeros[row_index];
      	  double sum = colorRhs[row_index];
      	  double* currentValues = colorValues[row_index];
      	  int* currentIdx = colorIdx[row_index];
      	  double* currentDiagonalIdx = colorDiagonal[row_index];

		  for (int j=currentNumberOfNonzeros-1; j>=0; j--) {
		    int curCol = currentIdx[j];
		    sum -= currentValues[j] * xv[curCol];
		  }
		  double currentDiagonal = *currentDiagonalIdx;
		  
		  sum += xv[row]*currentDiagonal; // Remove diagonal contribution from previous loop

		  xv[row] = sum/currentDiagonal;
        }
      }
    }
  }
}
*/



//computes one step of SYMGS using OMP and multi-coloring/dependency graph
void ComputeSYMGS_GC(LocalGSData* localData, DoubleVector x, int num_colors, int num_threads) {
  #pragma omp parallel firstprivate(localData, x, num_colors, num_threads)
  {  
  	double* xv = x.values;
  	int size = x.length;
  	
    for (int color=0; color<num_colors; color++) {
    
      #pragma omp for schedule(static, 1)
      for(int thread=0; thread<num_threads; thread++) {
      	LocalGSData threadData = localData[thread];
      	int* colorRows = threadData.rows[color];
      	int* colorNumberOfNonzeros = threadData.nonzerosInRow[color];
      	double* colorRhs = threadData.rhs[color];
      	double** colorValues = threadData.matrixValues[color];
      	int** colorIdx = threadData.mtxInd[color];
      	double* colorDiagonal = threadData.matrixDiagonal[color];
      	int numRows = threadData.numRows[color];
      	
      	for (int row_index=0; row_index<numRows; row_index++) { 
      	  int row = colorRows[row_index]; 
      	  int currentNumberOfNonzeros = colorNumberOfNonzeros[row_index];
      	  double sum = colorRhs[row_index];
      	  double* currentValues = colorValues[row_index];
      	  int* currentIdx = colorIdx[row_index];
      	  double currentDiagonal = colorDiagonal[row_index];

		  for (int j=0; j<currentNumberOfNonzeros; j++) {
		    int curCol = currentIdx[j];
		    sum -= currentValues[j] * xv[curCol];
		  }
		  sum += xv[row]*currentDiagonal; // Remove diagonal contribution from previous loop

		  xv[row] = sum/currentDiagonal;
        }
      }
    }

    // Now the back sweep.
    for(int color=num_colors-1; color>=0; color--) {
    
      #pragma omp for schedule(static, 1)
      for(int thread=0; thread<num_threads; thread++) {
      	LocalGSData threadData = localData[thread];
      	int* colorRows = threadData.rows[color];
      	int* colorNumberOfNonzeros = threadData.nonzerosInRow[color];
      	double* colorRhs = threadData.rhs[color];
      	double** colorValues = threadData.matrixValues[color];
      	int** colorIdx = threadData.mtxInd[color];
      	double* colorDiagonal = threadData.matrixDiagonal[color];
      	int numRows = threadData.numRows[color];
      	
      	for (int row_index=numRows-1; row_index>=0; row_index--) { 
      	  int row = colorRows[row_index]; 
      	  int currentNumberOfNonzeros = colorNumberOfNonzeros[row_index];
      	  double sum = colorRhs[row_index];
      	  double* currentValues = colorValues[row_index];
      	  int* currentIdx = colorIdx[row_index];
      	  double currentDiagonal = colorDiagonal[row_index];

		  for (int j=currentNumberOfNonzeros-1; j>=0; j--) {
		    int curCol = currentIdx[j];
		    sum -= currentValues[j] * xv[curCol];
		  }
		  sum += xv[row]*currentDiagonal; // Remove diagonal contribution from previous loop

		  xv[row] = sum/currentDiagonal;
        }
      }
    }
  }
}

