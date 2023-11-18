//contains functions to schedule row updates to different threads/pes

#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>

#include "../../include/hpcg_matrix.hpp"
#include "../../include/IntVectorArray.hpp"
#include "../../include/DoubleVector.hpp"
#include "coloring.cpp"

void sort_colors(HPCGMatrix A, IntVectorArray & colors);
void sort_colors_blocks(HPCGMatrix A, IntVectorArray & colors, int block_size);
double get_scheduling_quality(HPCGMatrix A, IntVectorArray assigned_rows[], int n_threads);
void reorder_colors_sw(IntVectorArray & colors, int num_threads);
bool assign_rows(HPCGMatrix & A, IntVectorArray & colors, IntVectorArray result[], int n_acc, int max_size);
bool assign_blocks(HPCGMatrix & A, IntVectorArray & colors, IntVectorArray result[], int n_acc, int block_size, int max_size);
void sort_ascending(IntVectorArray rows_by_thread[], int num_threads);
//void get_sw_scheduling(IntVectorArray rows_by_thread[], IntVectorArray result[], int num_threads);

//sorts rows for each color, such that for each color:
//  -rows with a higher number of nonzeros are first
//  -rows with the same number of nonzeros are in the order of their row index
void sort_colors(HPCGMatrix A, IntVectorArray & colors) {
    for(int color=0; color<colors.length; color++) {
        std::sort(colors.values[color].values,                                  
                  colors.values[color].values+colors.values[color].length,      
                  [A](int const &row1, int const &row2) { 
                        if(A.nonzerosInRow[row1] == A.nonzerosInRow[row2]) {
                            return row1 < row2;
                        }           
                        return A.nonzerosInRow[row1] > A.nonzerosInRow[row2]; 
                  });
        
    }
}

//same as sort_colors, but for blocks
void sort_colors_blocks(HPCGMatrix A, IntVectorArray & colors, int block_size) {
    int num_blocks = A.numberOfRows / block_size;
    int* nnz_blocks = new int[num_blocks];
    for(int block=0; block < num_blocks; block++) {
        nnz_blocks[block] = 0;
        for(int row=block*block_size; row < (block+1)*block_size; row++) {
            nnz_blocks[block] += A.nonzerosInRow[row];
        }
    }
    for(int color=0; color<colors.length; color++) {
        std::sort(colors.values[color].values,                                  
                  colors.values[color].values+colors.values[color].length,      
                  [nnz_blocks](int const &block1, int const &block2) { 
                        if(nnz_blocks[block1] == nnz_blocks[block2]) {
                            return block1 < block2;
                        }           
                        return nnz_blocks[block1] > nnz_blocks[block2]; 
                  });
        
    }
    delete[] nnz_blocks;
}


//returns the theoretical speed-up achieved with the current scheduling
double get_scheduling_quality(HPCGMatrix A, IntVectorArray assigned_rows[], int n_threads) {
    int critical_nnz = 0;
    for(int color=0; color<assigned_rows[0].length; color++) {
        int highest_nnz = 0;
        for(int th=0; th<n_threads; th++) {
            int nnz_th = 0;
            for(int row_index=0; row_index<assigned_rows[th].values[color].length; row_index++) {
                int row = assigned_rows[th].values[color].values[row_index];
                nnz_th += A.nonzerosInRow[row];
            }
            if(nnz_th > highest_nnz) {
                highest_nnz = nnz_th;
            }
        }
        critical_nnz += highest_nnz;
    }
    return ((double) A.numberOfNonzeros) / critical_nnz;
}

//changes the order of rows in a color in the following way: rows are split into groups of num_threads, odd-numbered groups are left untouched and even-numbered groups are reversed
//calling this function after sorting provides better scheduling quality
void reorder_colors_sw(IntVectorArray & colors, int num_threads) {
	for(int color=0; color<colors.length; color++) {
		int even = 1;
		for(int i=0; i<colors.values[i].length/num_threads; i++) {
			even = 1 - even;
			int offset = i*num_threads;
			if(even) {
				for(int j=0; j<num_threads/2; j++) {
					int temp = colors.values[i].values[offset+j];
					colors.values[i].values[offset+j] = colors.values[i].values[offset+num_threads-j-1];
					colors.values[i].values[offset+num_threads-j-1] = temp;
				}
			}
		}
	}
}

//result is an array with color and rows data for each PE: each element is an IntVectorArray with an IntVector of rows per color assigned to the PE
//rows are assigned in a greedy way based on the number of nonzeros, returns false if it cannot fit data with max_size words per pe, ignored if max_size is -1
bool assign_rows(HPCGMatrix & A, IntVectorArray & colors, IntVectorArray result[], int n_acc, int max_size) {
	int total_size_acc[n_acc];
    for(int acc=0; acc<n_acc; acc++) {
        InitializeIntVectorArr(result[acc], colors.length);
        total_size_acc[acc] = 3+colors.length;
    }
    for(int color=0; color<colors.length; color++) {
        int nnz_acc[n_acc]; //number of nonzeros of this color for each PE
        int num_rows_acc[n_acc]; //number of rows of this color for each PE
        int* rows_acc[n_acc]; //array of rows of this color for each PE (temporary)
        for(int i=0; i<n_acc; i++) {
            nnz_acc[i] = 0;
            rows_acc[i] = (int*) malloc(colors.values[color].length*sizeof(int));
            num_rows_acc[i] = 0;
        }
        for(int row=0; row<colors.values[color].length; row++) {
            int min_nnz = A.numberOfNonzeros;
            int acc_min_nnz = 0;
            int real_row = colors.values[color].values[row];
            
            bool found_not_full = false;
            for(int i=0; i<n_acc; i++) {
            	if(total_size_acc[i] + 2*A.nonzerosInRow[real_row]+3 <= max_size || max_size == -1) {
            		found_not_full = true;
            		if(nnz_acc[i] < min_nnz || (nnz_acc[i] == min_nnz && total_size_acc[i] < total_size_acc[acc_min_nnz])) {
		                min_nnz = nnz_acc[i];
		                acc_min_nnz = i;
		            }
            	}
            }
            if(!found_not_full) {
            	return false;
            }
            
            nnz_acc[acc_min_nnz] += A.nonzerosInRow[real_row];
            rows_acc[acc_min_nnz][num_rows_acc[acc_min_nnz]] = real_row;
            total_size_acc[acc_min_nnz] += 2*A.nonzerosInRow[real_row] + 3;
            num_rows_acc[acc_min_nnz]++;
        }
        for(int acc=0; acc<n_acc; acc++) {
            InitializeIntVector(result[acc].values[color], num_rows_acc[acc]);
            for(int row=0; row<num_rows_acc[acc]; row++) {
                result[acc].values[color].values[row] = rows_acc[acc][row];
            }
            free(rows_acc[acc]);
        }
    }
    /*
    for(int acc=0; acc<n_acc; acc++) {
    	std::cout << total_size_acc[acc] << "\n";
    }
    std::cout << "\n";
    */
    return true;
}

/*
//old version
//result is an array with color and rows data for each PE: each element is an IntVectorArray with an IntVector of rows per color assigned to the PE
//rows are assigned in a greedy way based on the number of nonzeros, returns false if it cannot fit data with max_size words per pe, ignored if max_size is -1
void assign_rows_old(HPCGMatrix & A, IntVectorArray & colors, IntVectorArray result[], int n_acc) {
    for(int acc=0; acc<n_acc; acc++) {
        InitializeIntVectorArr(result[acc], colors.length);
    }
    for(int color=0; color<colors.length; color++) {
        int nnz_acc[n_acc]; //number of nonzeros of this color for each PE
        int num_rows_acc[n_acc]; //number of rows of this color for each PE
        int* rows_acc[n_acc]; //array of rows of this color for each PE (temporary)
        for(int i=0; i<n_acc; i++) {
            nnz_acc[i] = 0;
            rows_acc[i] = (int*) malloc(colors.values[color].length*sizeof(int));
            num_rows_acc[i] = 0;
        }
        for(int row=0; row<colors.values[color].length; row++) {
            int min_nnz = A.numberOfNonzeros;
            int acc_min_nnz = 0;
            for(int i=0; i<n_acc; i++) {
                if(nnz_acc[i] < min_nnz) {
                    min_nnz = nnz_acc[i];
                    acc_min_nnz = i;
                }
            }
            int real_row = colors.values[color].values[row];
            nnz_acc[acc_min_nnz] += A.nonzerosInRow[real_row];
            rows_acc[acc_min_nnz][num_rows_acc[acc_min_nnz]] = real_row;
            num_rows_acc[acc_min_nnz]++;
        }
        for(int acc=0; acc<n_acc; acc++) {
            InitializeIntVector(result[acc].values[color], num_rows_acc[acc]);
            for(int row=0; row<num_rows_acc[acc]; row++) {
                result[acc].values[color].values[row] = rows_acc[acc][row];
            }
            free(rows_acc[acc]);
        }
    }
}
*/

//like assign_rows but each block is in the same PE
bool assign_blocks(HPCGMatrix & A, IntVectorArray & colors, IntVectorArray result[], int n_acc, int block_size, int max_size) {
    int num_colors = colors.length;
    int total_size_acc[n_acc];
    //account for block sizes that do not cover the entire matrix
    if(A.numberOfColumns % block_size != 0) {
        num_colors++;
    }
    for(int acc=0; acc<n_acc; acc++) {
        InitializeIntVectorArr(result[acc], num_colors);
        total_size_acc[acc] = 3+num_colors;
    }
    for(int color=0; color<colors.length; color++) {
        int nnz_acc[n_acc]; //number of nonzeros of this color for each PE
        int num_rows_acc[n_acc]; //number of rows of this color for each PE
        int* rows_acc[n_acc]; //array of rows of this color for each PE (temporary)
        for(int i=0; i<n_acc; i++) {
            nnz_acc[i] = 0;
            rows_acc[i] = (int*) malloc(colors.values[color].length*sizeof(int)*block_size);
            num_rows_acc[i] = 0;
        }
        for(int idx=0; idx<colors.values[color].length; idx++) {
            int min_nnz = A.numberOfNonzeros;
            int acc_min_nnz = 0;
            int block = colors.values[color].values[idx];
            int nnz_block = 0;
            for(int i=block*block_size; i<(block+1)*block_size; i++) {
                nnz_block += A.nonzerosInRow[i];
            }
            
            bool found_not_full = false;
            for(int i=0; i<n_acc; i++) {
            	if(total_size_acc[i] + 2*nnz_block + 3*block_size <= max_size || max_size == -1) {
            		found_not_full = true;
            		if(nnz_acc[i] < min_nnz || (nnz_acc[i] == min_nnz && total_size_acc[i] < total_size_acc[acc_min_nnz])) {
		                min_nnz = nnz_acc[i];
		                acc_min_nnz = i;
		            }
            	}
            }
            if(!found_not_full) {
            	return false;
            }

            nnz_acc[acc_min_nnz] += nnz_block;
            for(int i=0; i<block_size; i++) {
                rows_acc[acc_min_nnz][num_rows_acc[acc_min_nnz]+i] = block*block_size+i;
            }
            total_size_acc[acc_min_nnz] += 2*nnz_block + 3*block_size;
            num_rows_acc[acc_min_nnz] += block_size;
        }
        for(int acc=0; acc<n_acc; acc++) {
            InitializeIntVector(result[acc].values[color], num_rows_acc[acc]);
            for(int row=0; row<num_rows_acc[acc]; row++) {
                result[acc].values[color].values[row] = rows_acc[acc][row];
            }
            free(rows_acc[acc]);
        }
    }
    //the remaining values are put in the PE with the most space
    int freest_acc = 0;
    int freest_space = total_size_acc[0];
    for(int acc=0; acc<n_acc; acc++) {
    	if(total_size_acc[acc] < freest_space) {
    		freest_space = total_size_acc[acc];
    		freest_acc = acc;
    	}
    }
    if(A.numberOfColumns % block_size != 0) {
        InitializeIntVector(result[freest_acc].values[num_colors-1], A.numberOfColumns % block_size);
        int counter = 0;
        for(int row = (A.numberOfColumns / block_size)*block_size; row < A.numberOfColumns; row++) {
            result[freest_acc].values[num_colors-1].values[counter] = row;
            counter++;
            freest_space += 2*A.nonzerosInRow[row] + 3;
        }
        for(int i=0; i<n_acc; i++) {
        	if(i != freest_acc) {
        		InitializeIntVector(result[i].values[num_colors-1], 0);
        	}
        }
    }
    if(freest_space > max_size) {
    	return false;
    }
    return true;
}

//sorts rows/blocks for each thread and each color in ascending order
void sort_ascending(IntVectorArray rows_by_thread[], int num_threads) {
	for(int th=0; th<num_threads; th++) {
		for(int color=0; color<rows_by_thread[th].length; color++) {
	        std::sort(rows_by_thread[th].values[color].values, rows_by_thread[th].values[color].values+rows_by_thread[th].values[color].length);
		}
	}
}

/*
//changes data structure so that the outermost array divides rows by color instead of by thread
void get_sw_scheduling(IntVectorArray rows_by_thread[], IntVectorArray result[], int num_threads) {
    for(int color=0; color<rows_by_thread[0].length; color++) {
    	InitializeIntVectorArr(result[color], num_threads);
        for(int thread=0; thread<num_threads; thread++) {
         	InitializeIntVector(result[color].values[thread], rows_by_thread[thread].values[color].length);
        	for(int row=0; row<rows_by_thread[thread].values[color].length; row++) {
        		result[color].values[thread].values[row] = rows_by_thread[thread].values[color].values[row];
        	}
        }
    }
}
*/
