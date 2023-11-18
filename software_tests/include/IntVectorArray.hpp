//array of IntVectors

#ifndef INT_VECTOR_ARR_HPP
#define INT_VECTOR_ARR_HPP
#include <cassert>
#include <cstdlib>
#include <cmath>
#include "IntVector.hpp"

struct Int_Vector_Arr_STRUCT {
  int length;  //!< length of the vector
  IntVector * values;          //!< array of values
};
typedef struct Int_Vector_Arr_STRUCT IntVectorArray;

inline void InitializeIntVectorArr(IntVectorArray & arr, int length) {
  arr.length = length;
  arr.values = new IntVector[length];
  return;
}

inline void DeleteIntVectorArray(IntVectorArray & arr) {
  for(int i=0; i<arr.length; i++) {
    DeleteIntVector(arr.values[i]);
  }
  delete [] arr.values;
}

#endif
