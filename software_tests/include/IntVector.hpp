//Like DoubleVector, but using ints

#ifndef INT_VECTOR_HPP
#define INT_VECTOR_HPP
#include <cassert>
#include <cstdlib>
#include <cmath>

struct Int_Vector_STRUCT {
  int length;  //!< length of the vector
  int * values;          //!< array of values
};
typedef struct Int_Vector_STRUCT IntVector;

/*!
  Initializes input vector.

  @param[in] v
  @param[in] length Length of input vector
 */
inline void InitializeIntVector(IntVector & v, int length) {
  v.length = length;
  v.values = new int[length];
  return;
}

inline void PrintIntVector(const IntVector & v) {
  for(int i=0; i < v.length; i++) {
    printf("%d ", v.values[i]);
  }
  printf("\n");
}

inline void DeleteIntVector(IntVector & v) {
  delete [] v.values;
}

#endif
