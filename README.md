# PSyGS Gen: Parallel Symmetric Gauss-Seidel Generator

PSyGS Gen is a generator of domain specific architectures to execute
the symmetric Gauss-Seidel algorithm in parallel, exploiting
dependency-free data preprocessed with coloring algorithms.

| Content                                                      | Directory           |
|--------------------------------------------------------------|---------------------|
| chisel source code of the generator                          | src/main/scala      |
| runnable chisel tests of the generator                       | src/test/scala      |
| scripts to test the software implementation of parallel SyGS | software_tests/main |

## Requirements:
* Java Development Kit 11
* Scala 2.13.10
* Scala sbt

## Chisel tests
To run the chisel tests of the generator, run `sbt test`

## Software tests
To run the software tests, run
```
cd software_tests/main
make
./test_all_matrices maxThreads
```
where maxThreads is the maximum number of threads for the CPU to use
