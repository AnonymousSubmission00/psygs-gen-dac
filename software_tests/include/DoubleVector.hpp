
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
 @file Vector.hpp

 HPCG data structures for dense vectors
 */

#ifndef DOUBLE_VECTOR_HPP
#define DOUBLE_VECTOR_HPP

#include <cassert>
#include <cstdlib>
#include <cstdint>
#include <cmath>
#include <fstream>

#define SIZE_RANDOM 10

struct DoubleVector_STRUCT {
  int length;        //!< length of the vector
  double *values;    //!< array of values
};
typedef struct DoubleVector_STRUCT DoubleVector;

/*!
  Initializes input vector.

  @param[in] v
  @param[in] length Length of input vector
 */
inline void InitializeDoubleVector(DoubleVector & v, int length) {

  v.length = length;
  v.values = new double[length];
  for(int i=0; i<length; i++) {

    v.values[i] = 0.0;
  }
}

inline void FillRandomDoubleVector(DoubleVector & v, unsigned seed, double maxValue) {

  srand(seed);
  int length = v.length;
  double * vv = v.values;
  for (int i=0; i<length; ++i) {

      vv[i] = ((rand() / (double)(RAND_MAX))*2.0 - 1.0)*maxValue*SIZE_RANDOM;
      //printf("%ld\n", *(int64_t*) (vv + i));
  }
}

inline void InitializeDoubleVectorFromPath(DoubleVector & v, const std::string &path, int length) {

  std::ifstream infile;
  infile.open(path);

  int64_t x;
  std::string junk;

  InitializeDoubleVector(v, length);
  for(int i = 0; i < length; i++) {

    infile >> x >> junk;
    v.values[i] = *(double*) &x;
  }

  infile.close();
}

inline void ExportDoubleVector(DoubleVector & v, char* path) {

  std::ofstream outfile;
  outfile.open(path);

  for(int i = 0; i < v.length; i++)
    outfile << *(int64_t*) (v.values + i) << " " << v.values[i] << "\n";

  outfile.close();
}

inline void Diff(const DoubleVector & v1, const DoubleVector & v2, const DoubleVector & res) {

  for(int i=0; i<v1.length; i++)
      res.values[i] = v1.values[i] - v2.values[i];
}

inline double Abs(const DoubleVector & v) {

  double res = 0;
  for(int i=0; i<v.length; i++)
      res += v.values[i]*v.values[i];
  return sqrt(res);
}

inline void DeleteDoubleVector(DoubleVector & v) {
	delete [] v.values;
}

#endif
