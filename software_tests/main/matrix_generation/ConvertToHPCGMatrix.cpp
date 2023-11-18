#include "../../include/hpcg_matrix.hpp"
#include "GenerateHPCG.cpp"

HPCGMatrix convertToHPCGMatrix(const std::vector<int> &row_idx, const std::vector<int> &col_idx, const std::vector<double> &nonzeros);

HPCGMatrix generateHPCGMatrix(int nx, int ny, int nz) {
    std::vector<int> row_idx;
    std::vector<int> col_idx;
    std::vector<double> nonzeros;
    generate_hpcg_problem(nx, ny, nz, row_idx, col_idx, nonzeros);
    return convertToHPCGMatrix(row_idx, col_idx, nonzeros);
}

//from results of GenerateHPCG to HPCGMatrix
HPCGMatrix convertToHPCGMatrix(const std::vector<int> &row_idx, const std::vector<int> &col_idx, const std::vector<double> &nonzeros) {
    HPCGMatrix A;
    A.numberOfRows = row_idx.size() - 1;
    A.numberOfColumns = row_idx.size() - 1;
    A.numberOfNonzeros = nonzeros.size();

    A.nonzerosInRow = (int*) malloc(sizeof(int)*A.numberOfRows);
    A.mtxInd = (int**) malloc(sizeof(int*)*A.numberOfRows);
    A.matrixValues = (double**) malloc(sizeof(double*)*A.numberOfRows);
    A.matrixDiagonal = (double**) malloc(sizeof(double*)*A.numberOfRows);

    for(int row=0; row<A.numberOfRows; row++) {
        int start_row_idx = row_idx[row];
        int end_row_idx = row_idx[row+1];
        A.nonzerosInRow[row] = end_row_idx-start_row_idx;

        A.mtxInd[row] = (int*) malloc(sizeof(int)*A.nonzerosInRow[row]);
        A.matrixValues[row] = (double*) malloc(sizeof(double)*A.nonzerosInRow[row]);
        A.matrixDiagonal[row] = (double*) malloc(sizeof(double));

        int counter = 0;
        for(int i=start_row_idx; i<end_row_idx; i++) {
            int col = col_idx[i];
            A.mtxInd[row][counter] = col;
            A.matrixValues[row][counter] = nonzeros[i];
            if(col == row) {
                A.matrixDiagonal[row][0] = nonzeros[i];
            }
            counter++;
        }
    }

    return A;
}
