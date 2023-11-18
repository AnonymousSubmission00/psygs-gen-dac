//Utilities to get the current of clock cycle count

#ifdef _WIN32

//#include <intrin.h>   //VC
#include <x86intrin.h>  //GCC
uint64_t rdtsc() {

    return __rdtsc();
}

#else //Linux, Unix

#ifdef __arm64__
uint64_t rdtsc()
{
    uint64_t val;
    asm volatile("mrs %0, cntvct_el0" : "=r" (val));
    return val;
}
#else
uint64_t rdtsc() {

    unsigned int lo,hi;
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return ((uint64_t)hi << 32) | lo;
}
#endif

#endif

uint64_t get_clock_count() {

  return rdtsc();
}