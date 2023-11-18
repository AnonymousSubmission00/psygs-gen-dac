#include <iostream>
#include <sstream>
#include <unordered_map>
#include <chrono>

#include "ComputeSYMGS_ref.hpp"
#include "ComputeSYMGS_ref.cpp"
#include "ComputeSYMGS_GC.cpp"
#include "clock.hpp"
#include "preprocessing/scheduling.cpp"
#include "matrix_generation/ConvertToHPCGMatrix.cpp"

#define THRESHOLD 1e-11
#define CHECK_FREQ 1
#define MAX_ITERATIONS INT64_MAX
#define DEFAULT_BLOCK_SIZE 128

uint64_t computeGS(HPCGMatrix & A, DoubleVector & b, double threshold, int checkFreq);
uint64_t computeParallelGS(HPCGMatrix & A, DoubleVector & b, LocalGSData* localData, double threshold, int checkFreq, IntVectorArray colors[], int numThreads);
int estimateIterations(double old_remainder, double remainder, double target, int checkFreq, int old_expected_iter);
void preprocess(HPCGMatrix A, DoubleVector b, IntVectorArray initial_colors, IntVectorArray* divided_colors, int numThreads, LocalGSData* localData);
void preprocess_blocks(HPCGMatrix A, DoubleVector b, IntVectorArray initial_colors, IntVectorArray* divided_colors, int numThreads, LocalGSData* localData, int block_size);

void run_matrix_test(const std::string &mtx_path, const std::string &vec_path,
                     std::unordered_map<std::string, std::any> &gsRes,
                     std::unordered_map<std::string, std::any> &gcRes,
                     std::unordered_map<std::string, std::any> &bcRes,
                     std::unordered_map<std::string, std::any> &dgRes,
                     int maxThreads) {
    HPCGMatrix A;
    DoubleVector b;

    IntVector node_colors_1;
    IntVectorArray color_nodes_1;
    IntVector node_colors_2;
    IntVectorArray color_nodes_2;
    IntVector node_colors_3;
    IntVectorArray color_nodes_3;
    uint32_t blockSize;

    auto t0 = std::chrono::high_resolution_clock::now();
    auto t1 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> fs {};
    std::chrono::nanoseconds nanoseconds;

    double graphColoringOvh;
    double blockColoringOvh;
    double depGraphOvh;

    uint64_t stdIter;
    uint64_t gcIter;
    uint64_t bcIter;
    uint64_t dgIter;

    double theoreticalSpeedup;

	if(mtx_path.rfind("hpcg", 0) == 0) {
		int dim = std::stoi(mtx_path.substr(4));
		A = generateHPCGMatrix(dim, dim, dim);
	} else {
    	InitializeHPCGMatrix(A, mtx_path.c_str());
    }
    InitializeDoubleVectorFromPath(b, vec_path, A.numberOfColumns);

    if(A.numberOfRows >= 64 * DEFAULT_BLOCK_SIZE) blockSize = DEFAULT_BLOCK_SIZE;
    else blockSize = A.numberOfRows / 64;
    
    if(blockSize < 1) blockSize = 1;  

    std::cout << "Initializing color vectors\n";
    fflush(stdout);

    t0 = std::chrono::high_resolution_clock::now();
    computeGraphColoring(A, node_colors_1, color_nodes_1);
    sort_colors(A, color_nodes_1);
    t1 = std::chrono::high_resolution_clock::now();
    fs = t1 - t0;
    nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(fs);
    graphColoringOvh = nanoseconds.count() / 1e9;

    t0 = std::chrono::high_resolution_clock::now();
    computeBlockColoring(A, blockSize, node_colors_2, color_nodes_2);
    sort_colors_blocks(A, color_nodes_2, blockSize);
    t1 = std::chrono::high_resolution_clock::now();
    fs = t1 - t0;
    nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(fs);
    blockColoringOvh = nanoseconds.count() / 1e9;

    t0 = std::chrono::high_resolution_clock::now();
    computeDepGraph(A, node_colors_3, color_nodes_3);
    sort_colors(A, color_nodes_3);
    t1 = std::chrono::high_resolution_clock::now();
    fs = t1 - t0;
    nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(fs);
    depGraphOvh = nanoseconds.count() / 1e9;

    std::cout << "\nRunning Gauss-Seidel\n";
    fflush(stdout);

    t0 = std::chrono::high_resolution_clock::now();
    uint64_t beforeClockStd = get_clock_count();
    stdIter = computeGS(A, b, THRESHOLD, CHECK_FREQ);
    t1 = std::chrono::high_resolution_clock::now();
    uint64_t afterClockStd = get_clock_count();
    uint64_t diffClockStd = afterClockStd - beforeClockStd;
    fs = t1 - t0;
    nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(fs);
    double stdTime = nanoseconds.count() / 1e9;
    gsRes["time"] = std::any_cast<double>(gsRes["time"]) + stdTime;
    gsRes["iterations"] = std::any_cast<double>(gsRes["iterations"]) + stdIter;
    gsRes["clock cycles"] = std::any_cast<double>(gsRes["clock cycles"])+ diffClockStd;
    gsRes["frequency"] = std::any_cast<double>(gsRes["frequency"]) + (diffClockStd / (double) stdTime) / 1e6;
    
    //std::cout << "standard time: " << stdTime << std::endl;

    for(int numThreads = 1; numThreads <= maxThreads; numThreads *= 2) {
        std::cout << "Running Gauss-Seidel with graph coloring: " << numThreads << " threads\n";
        fflush(stdout);
        
        t0 = std::chrono::high_resolution_clock::now();
        LocalGSData localData[numThreads];
        //ContinousLocalGSData contLocalData[numThreads];
        IntVectorArray divided_colors[numThreads];
        preprocess(A, b, color_nodes_1, divided_colors, numThreads, localData);
        t1 = std::chrono::high_resolution_clock::now();
        fs = t1 - t0;
        double totalGraphColoringOvh = graphColoringOvh + std::chrono::duration_cast<std::chrono::nanoseconds>(fs).count() / 1e9;
        theoreticalSpeedup = get_scheduling_quality(A, divided_colors, numThreads);

		uint64_t beforeClock = get_clock_count();
		t0 = std::chrono::high_resolution_clock::now();
        gcIter = computeParallelGS(A, b, localData, THRESHOLD, CHECK_FREQ, divided_colors, numThreads);
        t1 = std::chrono::high_resolution_clock::now();
        uint64_t afterClock = get_clock_count();
        uint64_t diffClock = afterClock - beforeClock;
        fs = t1 - t0;
        nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(fs);
        double gcTime = nanoseconds.count() / 1e9;
        
        //std::cout << "time: " << gcTime << std::endl;
        
        for(int i=0; i<numThreads; i++) {
		  FreeLocalGSData(localData[i], divided_colors[i]);
		  //FreeContinousLocalGSData(contLocalData[i]);
		  DeleteIntVectorArray(divided_colors[i]);
		}

        std::stringstream ss;
        ss << numThreads << " threads";
        std::string threadString = ss.str();
        auto threadRes = std::any_cast<std::shared_ptr<std::unordered_map<std::string, std::any>>>(gcRes[threadString]);

        (*threadRes)["time"]                   = std::any_cast<double>((*threadRes)["time"])                + gcTime;
        (*threadRes)["clock cycles"]           = std::any_cast<double>((*threadRes)["clock cycles"])        + diffClock;
        (*threadRes)["iterations"]             = std::any_cast<double>((*threadRes)["iterations"])          + gcIter;
        (*threadRes)["speedup"]                = std::any_cast<double>((*threadRes)["speedup"])             + stdTime / (double) (gcTime + totalGraphColoringOvh);
        (*threadRes)["theoretical speedup"]    = std::any_cast<double>((*threadRes)["theoretical speedup"]) + theoreticalSpeedup;
        (*threadRes)["overhead"]               = std::any_cast<double>((*threadRes)["overhead"])            + totalGraphColoringOvh / (double) stdTime;
        (*threadRes)["frequency"]              = std::any_cast<double>((*threadRes)["frequency"])           + (diffClock / (double) gcTime) / 1e6;
    }

    for(int numThreads = 1; numThreads <= maxThreads; numThreads *= 2) {
        std::cout << "Running Gauss-Seidel with block coloring: " << numThreads << " threads, block size = " << blockSize << "\n";
        fflush(stdout);

        t0 = std::chrono::high_resolution_clock::now();
        LocalGSData localData[numThreads];
        //ContinousLocalGSData contLocalData[numThreads];
        IntVectorArray divided_colors[numThreads];
        preprocess_blocks(A, b, color_nodes_2, divided_colors, numThreads, localData, blockSize);
        t1 = std::chrono::high_resolution_clock::now();
        fs = t1 - t0;
        double totalBlockColoringOvh = blockColoringOvh + std::chrono::duration_cast<std::chrono::nanoseconds>(fs).count() / 1e9;
        theoreticalSpeedup = get_scheduling_quality(A, divided_colors, numThreads);
		
		uint64_t beforeClock = get_clock_count();
		t0 = std::chrono::high_resolution_clock::now();
        bcIter = computeParallelGS(A, b, localData, THRESHOLD, CHECK_FREQ, divided_colors, numThreads);
        t1 = std::chrono::high_resolution_clock::now();
        uint64_t afterClock = get_clock_count();
        uint64_t diffClock = afterClock - beforeClock;
        fs = t1 - t0;
        nanoseconds = std::chrono::duration_cast<std::chrono::nanoseconds>(fs);
        double bcTime = nanoseconds.count() / 1e9;
        
        for(int i=0; i<numThreads; i++) {
		  FreeLocalGSData(localData[i], divided_colors[i]);
		  //FreeContinousLocalGSData(contLocalData[i]);
		  DeleteIntVectorArray(divided_colors[i]);
		}

		bcRes["block size"] = blockSize;
        std::stringstream ss;
        ss << numThreads << " threads";
        std::string threadString = ss.str();
        auto threadRes = std::any_cast<std::shared_ptr<std::unordered_map<std::string, std::any>>>(bcRes[threadString]);

        (*threadRes)["time"]                   = std::any_cast<double>((*threadRes)["time"])                + bcTime;
        (*threadRes)["clock cycles"]           = std::any_cast<double>((*threadRes)["clock cycles"])        + diffClock;
        (*threadRes)["iterations"]             = std::any_cast<double>((*threadRes)["iterations"])          + bcIter;
        (*threadRes)["speedup"]                = std::any_cast<double>((*threadRes)["speedup"])             + stdTime / (double) (bcTime + blockColoringOvh);
        (*threadRes)["theoretical speedup"]    = std::any_cast<double>((*threadRes)["theoretical speedup"]) + theoreticalSpeedup;
        (*threadRes)["overhead"]               = std::any_cast<double>((*threadRes)["overhead"])            + totalBlockColoringOvh / (double) stdTime;
        (*threadRes)["frequency"]              = std::any_cast<double>((*threadRes)["frequency"])           + (diffClock / (double) bcTime) / 1e6;
    }

    for(int numThreads = 1; numThreads <= maxThreads; numThreads *= 2) {
        std::cout << "Running Gauss-Seidel with dependency graph: " << numThreads << " threads\n";
        fflush(stdout);
        
        t0 = std::chrono::high_resolution_clock::now();
        LocalGSData localData[numThreads];
        //ContinousLocalGSData contLocalData[numThreads];
        IntVectorArray divided_colors[numThreads];
        preprocess(A, b, color_nodes_3, divided_colors, numThreads, localData);
        t1 = std::chrono::high_resolution_clock::now();
        fs = t1 - t0;
        double totalDepGraphOvh = depGraphOvh + std::chrono::duration_cast<std::chrono::nanoseconds>(fs).count() / 1e9;
        theoreticalSpeedup = get_scheduling_quality(A, divided_colors, numThreads);
        
        uint64_t beforeClock = get_clock_count();
        t0 = std::chrono::high_resolution_clock::now();
        dgIter = computeParallelGS(A, b, localData, THRESHOLD, CHECK_FREQ, divided_colors, numThreads);
        t1 = std::chrono::high_resolution_clock::now();
        uint64_t afterClock = get_clock_count();
        uint64_t diffClock = afterClock - beforeClock;
        fs = t1 - t0;
        nanoseconds = std::chrono::duration_cast< std::chrono::nanoseconds>(fs);
        double dgTime = nanoseconds.count() / 1e9;
        
        for(int i=0; i<numThreads; i++) {
		  FreeLocalGSData(localData[i], divided_colors[i]);
		  //FreeContinousLocalGSData(contLocalData[i]);
		  DeleteIntVectorArray(divided_colors[i]);
		}
        
        //std::cout << "time: " << dgTime << std::endl;

        std::stringstream ss;
        ss << numThreads << " threads";
        std::string threadString = ss.str();
        auto threadRes = std::any_cast<std::shared_ptr<std::unordered_map<std::string, std::any>>>(dgRes[threadString]);

        (*threadRes)["time"]                   = std::any_cast<double>((*threadRes)["time"])                + dgTime;
        (*threadRes)["clock cycles"]           = std::any_cast<double>((*threadRes)["clock cycles"])        + diffClock;
        (*threadRes)["iterations"]             = std::any_cast<double>((*threadRes)["iterations"])          + dgIter;
        (*threadRes)["speedup"]                = std::any_cast<double>((*threadRes)["speedup"])             + stdTime / (double) (dgTime + totalDepGraphOvh);
        (*threadRes)["theoretical speedup"]    = std::any_cast<double>((*threadRes)["theoretical speedup"]) + theoreticalSpeedup;
        (*threadRes)["overhead"]               = std::any_cast<double>((*threadRes)["overhead"])            + totalDepGraphOvh / (double) stdTime;
        (*threadRes)["frequency"]              = std::any_cast<double>((*threadRes)["frequency"])           + (diffClock / (double) dgTime) / 1e6;
    }
    
    DeleteHPCGMatrix(A);
    DeleteDoubleVector(b);
    DeleteIntVectorArray(color_nodes_1);
    DeleteIntVectorArray(color_nodes_2);
    DeleteIntVectorArray(color_nodes_3);
    DeleteIntVector(node_colors_1);
    DeleteIntVector(node_colors_2);
    DeleteIntVector(node_colors_3);
}

/**
 * Computes Gauss-Seidel with matrix A and DoubleVector b.
 * Checks convergence every checkFreq iterations.
 * Convergence is considered reached if the module of the remainder is less than threshold times the module of b.
 * @return the total iterations
 */
uint64_t computeGS(HPCGMatrix &A, DoubleVector &b, double threshold, int checkFreq) {

  DoubleVector x;       //solution approximation
  DoubleVector mult;    //Ax
  DoubleVector diff;    //Ax - b
  InitializeDoubleVector(x, A.numberOfColumns);
  InitializeDoubleVector(mult, A.numberOfColumns);
  InitializeDoubleVector(diff, A.numberOfColumns);


  double remainder;
  double old_remainder = -1;
  uint64_t iterations = 0;
  int expected_iter = checkFreq;
  int old_expected_iter;
  double target = threshold * Abs(b);

  while(iterations < MAX_ITERATIONS) {

    for(int i = 0; i < expected_iter; i++) {

      ComputeSYMGS_ref(A, b, x);
      iterations++;
    }

    SpMV(A, x, mult);
    Diff(mult, b, diff);
    remainder = Abs(diff);

    if (remainder < target) {
      DeleteDoubleVector(x);
      DeleteDoubleVector(mult);
      DeleteDoubleVector(diff);
      return iterations;
    }
      

    //remainder is estimated to decrease exponentially
    old_expected_iter = expected_iter;
    expected_iter = estimateIterations(old_remainder, remainder, target, checkFreq, old_expected_iter);
    if(expected_iter > MAX_ITERATIONS - iterations)
        expected_iter = MAX_ITERATIONS - iterations;
    old_remainder = remainder;

    //std::cout << "Current iterations: " << iterations << ", Remainder: " << remainder << " Target: " << target << " Expected iterations: " << expected_iter << std::endl;
  }

  DeleteDoubleVector(x);
  DeleteDoubleVector(mult);
  DeleteDoubleVector(diff);
  return -1;
}

//the same as computeGS, but with a colors DoubleVector and with ComputeSYMGS_GC instead
uint64_t computeParallelGS(HPCGMatrix & A, DoubleVector & b, LocalGSData* localData, double threshold, int checkFreq, IntVectorArray colors[], int numThreads) {

  DoubleVector x;       //solution approximation
  DoubleVector mult;    //Ax
  DoubleVector diff;    //Ax - b
  InitializeDoubleVector(x, A.numberOfColumns);
  InitializeDoubleVector(mult, A.numberOfColumns);
  InitializeDoubleVector(diff, A.numberOfColumns);

  double remainder;
  double old_remainder = -1;
  uint64_t iterations = 0;
  int expected_iter = checkFreq;
  int old_expected_iter;
  double target = threshold * Abs(b);
  
  omp_set_num_threads(numThreads);
  omp_set_dynamic(0);
  
  while(iterations < MAX_ITERATIONS) {

    for(int i=0; i<expected_iter; i++) {

      ComputeSYMGS_GC(localData, x, colors[0].length, numThreads);
      iterations++;
    }

    SpMV(A, x, mult);
    Diff(mult, b, diff);
    remainder = Abs(diff);

    if (remainder < target) {
      DeleteDoubleVector(x);
      DeleteDoubleVector(mult);
      DeleteDoubleVector(diff);
      return iterations;
    }

    //remainder is estimated to decrease exponentially
    old_expected_iter = expected_iter;
    expected_iter = estimateIterations(old_remainder, remainder, target, checkFreq, old_expected_iter);
    if(expected_iter > MAX_ITERATIONS - iterations)
        expected_iter = MAX_ITERATIONS - iterations;
    old_remainder = remainder;

    //std::cout << "Current iterations: " << iterations << ", Remainder: " << remainder << " Target: " << target << " Expected iterations: " << expected_iter << std::endl;
  }

  DeleteDoubleVector(x);
  DeleteDoubleVector(mult);
  DeleteDoubleVector(diff);
  return -1;
}

//estimates the number of iterations for convergence
int estimateIterations(double old_remainder, double remainder, double target, int checkFreq, int old_expected_iter) {
	if(old_remainder == -1 || old_remainder <= remainder)
		return checkFreq;

	int result = (int) (log(remainder / target) / log(old_remainder / remainder) * old_expected_iter);

	if(result == 0)
		return 1;

	return result;
}

//performs scheduling and reordering
void preprocess(HPCGMatrix A, DoubleVector b, IntVectorArray initial_colors, IntVectorArray* divided_colors, int numThreads, LocalGSData* localData) {
    sort_colors(A, initial_colors);
    assign_rows(A, initial_colors, divided_colors, numThreads, -1);
    sort_ascending(divided_colors, numThreads);
    for(int i=0; i<numThreads; i++) {
	  getLocalGSData(localData[i], A, b, divided_colors[i]);
	  //getContinousLocalGSData(localData[i], contLocalData[i], divided_colors[i].length, A.numberOfRows, A.numberOfNonzeros);
	}
}

//same as preprocess, but for block coloring
void preprocess_blocks(HPCGMatrix A, DoubleVector b, IntVectorArray initial_colors, IntVectorArray* divided_colors, int numThreads, LocalGSData* localData, int block_size) {
    sort_colors(A, initial_colors);
    assign_blocks(A, initial_colors, divided_colors, numThreads, block_size, -1);
    sort_ascending(divided_colors, numThreads);
    for(int i=0; i<numThreads; i++) {
	  getLocalGSData(localData[i], A, b, divided_colors[i]);
	  //getContinousLocalGSData(localData[i], contLocalData[i], divided_colors[i].length, A.numberOfRows, A.numberOfNonzeros);
	}
}
