#ifndef LocalGSData_HPP
#define LocalGSData_HPP

#include <cassert>
#include <cstdlib>
#include <cmath>
#include <vector>
#include "hpcg_matrix.hpp"
#include "IntVectorArray.hpp"

struct LocalGSData_STRUCT {
	//array with the number of rows with color i at position i
	int * numRows;
	//matrix with the j-th row with color i at position (i,j)
	int ** rows;
	//matrix with the number of nonzeros in the j-th row with color i at position (i,j)
	int ** nonzerosInRow;
	//3D matrix with the column index of the k-th element of the j-th row of color i at position (i,j,k)
	int *** mtxInd;
	//3D matrix with the value of the k-th element of the j-th row of color i at position (i,j,k)
	double *** matrixValues;
	//matrix with the diagonal value of the matrix for the j-th row of color i at position (i,j)
	double ** matrixDiagonal;
	//matrix with the value of the rhs for the j-th row of color i at position (i,j)
	double ** rhs;
};
typedef struct LocalGSData_STRUCT LocalGSData;

void getLocalGSData(LocalGSData & LocalData, HPCGMatrix A, DoubleVector b, IntVectorArray colors) {
	LocalData.numRows = (int*) malloc(sizeof(int)*colors.length);
	LocalData.rows = (int**) malloc(sizeof(int*)*colors.length);
	LocalData.nonzerosInRow = (int**) malloc(sizeof(int*)*colors.length);
	LocalData.matrixDiagonal = (double**) malloc(sizeof(double*)*colors.length);
	LocalData.rhs = (double**) malloc(sizeof(double*)*colors.length);
	LocalData.mtxInd = (int***) malloc(sizeof(int**)*colors.length);
	LocalData.matrixValues = (double***) malloc(sizeof(double**)*colors.length);
	for(int color=0; color<colors.length; color++) {
		LocalData.numRows[color] = colors.values[color].length;
		LocalData.rows[color] = (int*) malloc(sizeof(int)*colors.values[color].length);
		LocalData.nonzerosInRow[color] = (int*) malloc(sizeof(int)*colors.values[color].length);
		LocalData.matrixDiagonal[color] = (double*) malloc(sizeof(double)*colors.values[color].length);
		LocalData.rhs[color] = (double*) malloc(sizeof(double)*colors.values[color].length);
		LocalData.mtxInd[color] = (int**) malloc(sizeof(int*)*colors.values[color].length);
		LocalData.matrixValues[color] = (double**) malloc(sizeof(double*)*colors.values[color].length);
		for(int row_index=0; row_index<colors.values[color].length; row_index++) {
			LocalData.rows[color][row_index] = colors.values[color].values[row_index];
			LocalData.nonzerosInRow[color][row_index] = A.nonzerosInRow[colors.values[color].values[row_index]];
			LocalData.matrixDiagonal[color][row_index] = A.matrixDiagonal[colors.values[color].values[row_index]][0];
			LocalData.rhs[color][row_index] = b.values[colors.values[color].values[row_index]];
			LocalData.mtxInd[color][row_index] = (int*) malloc(sizeof(int)*A.nonzerosInRow[colors.values[color].values[row_index]]);
			LocalData.matrixValues[color][row_index] = (double*) malloc(sizeof(double)*A.nonzerosInRow[colors.values[color].values[row_index]]);
			for(int column_index=0; column_index<A.nonzerosInRow[colors.values[color].values[row_index]]; column_index++) {
				LocalData.mtxInd[color][row_index][column_index] = A.mtxInd[colors.values[color].values[row_index]][column_index];
				LocalData.matrixValues[color][row_index][column_index] = A.matrixValues[colors.values[color].values[row_index]][column_index];
			}
		}
	}
}

void FreeLocalGSData(LocalGSData & LocalData, IntVectorArray colors) {
	free(LocalData.numRows);
	for(int color=0; color<colors.length; color++) {
		free(LocalData.rows[color]);
		free(LocalData.nonzerosInRow[color]);
		free(LocalData.matrixDiagonal[color]);
		free(LocalData.rhs[color]);
		for(int row_index=0; row_index<colors.values[color].length; row_index++) {
			free(LocalData.mtxInd[color][row_index]);
			free(LocalData.matrixValues[color][row_index]);
		}
		free(LocalData.mtxInd[color]);
		free(LocalData.matrixValues[color]);
	}
}

struct ContinousLocalGSData_STRUCT {
	//array with the number of rows with color i at position i
	int * numRows;
	
	//array with all data previously in rows, but continous
	int * rows_data;
	//array with pointers to data in rows_data for each color 
	int ** rows_idx;
	
	//array with all data previously in nonzerosInRow, but continous
	int * nonzerosInRow_data;
	//array with pointers to data in nonzerosInRow_data for each color 
	int ** nonzerosInRow_idx;
	
	//array with all data previously in rhs, but continous
	double * rhs_data;
	//array with pointers to data in rhs_data for each color 
	double ** rhs_idx;
	
	//array with all data previously in mtxInd, but continous
	int * mtxInd_data;
	//array with pointers to data in mtxInd_data for each color and row
	int ** mtxInd_idx_data;
	//array with pointers to data in mtxInd_idx_data for each color
	int *** mtxInd_idx_idx;
	
	//array with all data previously in matrixValues, but continous
	double * matrixValues_data;
	//array with pointers to data in matrixValues_data for each color and row
	double ** matrixValues_idx_data;
	//array with pointers to data in matrixValues_idx_data for each color
	double *** matrixValues_idx_idx;
	
	//array with all data previously in matrixDiagonal (pointers instead of values), but continous
	double ** matrixDiagonal_data;
	//array with pointers to data in matrixDiagonal_data for each color
	double *** matrixDiagonal_idx;

};
typedef struct ContinousLocalGSData_STRUCT ContinousLocalGSData;

void getContinousLocalGSData(LocalGSData disc, ContinousLocalGSData & cont, int num_colors, int num_rows, int nnz) {
	cont.numRows = (int*) malloc(sizeof(int)*num_colors);
	cont.rows_data = (int*) malloc(sizeof(int)*num_rows);
	cont.rows_idx = (int**) malloc(sizeof(int*)*num_colors);
	cont.nonzerosInRow_data = (int*) malloc(sizeof(int)*num_rows);
	cont.nonzerosInRow_idx = (int**) malloc(sizeof(int*)*num_colors);
	cont.rhs_data = (double*) malloc(sizeof(double)*num_rows);
	cont.rhs_idx = (double**) malloc(sizeof(double*)*num_colors);
	cont.mtxInd_data = (int*) malloc(sizeof(int)*nnz);
	cont.mtxInd_idx_data = (int**) malloc(sizeof(int*)*num_rows);
	cont.mtxInd_idx_idx = (int***) malloc(sizeof(int**)*num_colors);
	cont.matrixValues_data = (double*) malloc(sizeof(double)*nnz);
	cont.matrixValues_idx_data = (double**) malloc(sizeof(double*)*num_rows);
	cont.matrixValues_idx_idx = (double***) malloc(sizeof(double**)*num_colors);
	cont.matrixDiagonal_data = (double**) malloc(sizeof(double*)*num_rows);
	cont.matrixDiagonal_idx = (double***) malloc(sizeof(double**)*num_colors);
	
	int curr_rows = 0;
	int curr_nnz = 0;
	for(int color=0; color<num_colors; color++) {
		cont.numRows[color] = disc.numRows[color];
		
		cont.rows_idx[color] = cont.rows_data+curr_rows;
		cont.nonzerosInRow_idx[color] = cont.nonzerosInRow_data+curr_rows;
		cont.rhs_idx[color] = cont.rhs_data+curr_rows;
		cont.mtxInd_idx_idx[color] = cont.mtxInd_idx_data+curr_rows;
		cont.matrixValues_idx_idx[color] = cont.matrixValues_idx_data+curr_rows;
		cont.matrixDiagonal_idx[color] = cont.matrixDiagonal_data+curr_rows;
		
		for(int row_idx=0; row_idx<disc.numRows[color]; row_idx++) {
			int diagonal_idx = 0;
			cont.rows_data[curr_rows] = disc.rows[color][row_idx];
			cont.nonzerosInRow_data[curr_rows] = disc.nonzerosInRow[color][row_idx];
			cont.rhs_data[curr_rows] = disc.rhs[color][row_idx];
			
			cont.mtxInd_idx_data[curr_rows] = cont.mtxInd_data+curr_nnz;
			cont.matrixValues_idx_data[curr_rows] = cont.matrixValues_data+curr_nnz;
			
			for(int nz_idx=0; nz_idx<disc.nonzerosInRow[color][row_idx]; nz_idx++) {
				if(disc.mtxInd[color][row_idx][nz_idx] == disc.rows[color][row_idx]) {
					diagonal_idx = curr_nnz;
				}
				cont.mtxInd_data[curr_nnz] = disc.mtxInd[color][row_idx][nz_idx];
				cont.matrixValues_data[curr_nnz] = disc.matrixValues[color][row_idx][nz_idx];
				
				curr_nnz++;
			}
			
			cont.matrixDiagonal_data[curr_rows] = cont.matrixValues_data+diagonal_idx;
			
			curr_rows++;
		}
	}
}

void FreeContinousLocalGSData(ContinousLocalGSData & LocalData) {
	free(LocalData.numRows);
	free(LocalData.rows_data);
	free(LocalData.rows_idx);
	free(LocalData.nonzerosInRow_data);
	free(LocalData.nonzerosInRow_idx);
	free(LocalData.rhs_data);
	free(LocalData.rhs_idx);
	free(LocalData.matrixDiagonal_data);
	free(LocalData.matrixDiagonal_idx);
	free(LocalData.mtxInd_data);
	free(LocalData.mtxInd_idx_data);
	free(LocalData.mtxInd_idx_idx);
	free(LocalData.matrixValues_data);
	free(LocalData.matrixValues_idx_data);
	free(LocalData.matrixValues_idx_idx);
}

#endif // LocalHPCGMATRIX_HPP
