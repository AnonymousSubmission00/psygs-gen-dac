#ifndef GENERATE_HPCG_PROBLEM_H
#define GENERATE_HPCG_PROBLEM_H

#include <vector>
#include <tuple>
#include <iostream>
#include <array>

int get1DIndex(int x, int y, int z, int nx, int ny) {
  return (z * nx * ny) + (y * nx) + x;
}

std::tuple<int,int,int> get3DIndex(int flatIdx, int nx, int ny) {
    int idx = flatIdx;
    int z = idx / (nx * ny);
    idx = idx - (z * nx * ny);
    int y = idx / nx;
    int x = idx % nx;
    return std::tuple<int,int,int>{x,y,z};
}

void generate_hpcg_problem(int nx, int ny, int nz, std::vector<int> &row_idx, std::vector<int> &col_idx, std::vector<double> &nonzeros) {
  int nrows = nx * ny * nz;
  int ncols = nx * ny * nz;
  std::vector<std::vector<double>> denseA;
  denseA.reserve(nrows);
  for(int i=0; i<nrows; i++) {
    denseA.emplace_back(std::vector<double>{});
    denseA[i].reserve(ncols);
    for(int j=0; j<nrows; j++) {
      denseA[i].emplace_back(0.0);
    }
  }

  //std::cout << "Generating HPCG-like (" + nx << "," << ny << "," << nz << ") problem\n";

  // A cell has a non-zero at 1d(3d(A[i]) - 1[x|y|z]) if in range
  int nnz = 0;
  for(int i=0; i<nrows; i++){
    std::array<std::array<std::array<double,27>,27>,27> halo;
    std::tuple<int,int,int> index3D = get3DIndex(i, nx, ny);

    //print("Elem i=" + i + "  (" + myx + "," + myy + "," + myz + ") \n")
    denseA[i][i] = 1.0;
    nnz++;
    for(int dx=-1; dx<2; dx++) {
      for(int dy=-1; dx<2; dx++) {
        for(int dz=-1; dx<2; dx++) {
          if(!(dx == 0 && dy == 0 && dz == 0)){
            int neighx = std::get<0>(index3D) + dx;
            int neighy = std::get<1>(index3D) + dy;
            int neighz = std::get<2>(index3D) + dz;
            //print("Elem i neigh at (" + neighx + "," + neighy + "," + neighz + ")\n")
            if(neighx >= 0 && neighx < nx && 
              neighy >= 0 && neighy < ny &&
              neighz >= 0 && neighz < nz) {
              denseA[i][get1DIndex(neighx, neighy, neighz, nx, ny)] = 1.0;
              nnz++;
              //print("Elem i=" + i + "  (" + myx + "," + myy + "," + myz + ") has nz at (" + neighx + "," + neighy + "," + neighz + ")\n")
            } else {
              //print("Elem i=" + i + "  (" + myx + "," + myy + "," + myz + ") INVALID at (" + neighx + "," + neighy + "," + neighz + ")\n")
            }
          }
        }
      } 
    }
    //std::cout << "row " << i << " has " << nnz_row << " + 1 nz\n";

  }
  //std::cout << "Generated " << nnz << " non zeros...\n";

  row_idx.reserve(nrows);
  col_idx.reserve(nnz);
  nonzeros.reserve(nnz);
  row_idx.emplace_back(0);
  int nnz_seen = 0;
  for(int i=0; i<nrows; i++){
    for(int j=0; j<ncols; j++) {
      if(denseA[i][j] > 0){
        col_idx.emplace_back(j);
        nonzeros.emplace_back(denseA[i][j]);
        nnz_seen++;
      }
    }
    row_idx.emplace_back(nnz_seen);
  }
}

#endif