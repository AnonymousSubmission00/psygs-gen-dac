
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

#ifndef COMPUTESYMGS_REF_HPP
#define COMPUTESYMGS_REF_HPP
#include "../include/hpcg_matrix.hpp"
#include "../include/DoubleVector.hpp"

int ComputeSYMGS_ref( const HPCGMatrix  & A, const DoubleVector & r, DoubleVector & x);

#endif // COMPUTESYMGS_REF_HPP
