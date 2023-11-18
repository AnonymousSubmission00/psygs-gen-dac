//tests the specified matrices

#include <string>
#include <vector>
#include <unordered_map>
#include <any>
#include <iostream>
#include <memory>

#include "run_matrix_test.cpp"

int main(int argc, char* argv[]) {

    if(argc != 3 && argc != 2) {

        std::cout << "Incorrect number of arguments!\n1 - maximum threads\n2 - resources directory (optional)";
        return 0;
    }
    std::string resDir;
    if(argc == 3) {
    	resDir = argv[1];
    } else {
    	resDir = "../../resources";
    }

    int testsPerMatrix = 1;
    int maxThreads = atoi(argv[1]);
    std::vector<std::string> matrices {"494_bus", "1138_bus", "add20", "add32", "bcsstk14", "bcsstk15", "bcsstk27", "fs_760_2"};

    //-------------------------------------------------------------------MATRIX SETS-------------------------------------------------------------------//
    //{"hpcg16"}
    //{"494_bus", "1138_bus", "add20", "add32", "bcsstk14", "bcsstk15", "bcsstk27", "fs_760_2"};
    //{"100k_700k/bodyy4", "100k_700k/bodyy5", "100k_700k/bodyy6", "100k_700k/Dubcova1", "100k_700k/jnlbrng1", "100k_700k/bcsstk18", "100k_700k/cbuckle"};
    //{"1M_5M/Dubcova2", "1M_5M/Dubcova3"};
    //{"8M_more/atmosmodl", "8M_more/atmosmodj", "8M_more/atmosmodm", "8M_more/atmosmodd"};
    //-------------------------------------------------------------------------------------------------------------------------------------------------//

    for(const auto &matrix : matrices) {
    	std::string mtxPath;
		if(matrix.rfind("hpcg", 0) == 0) {
			mtxPath = matrix;
		} else {
			mtxPath = resDir + "/matrices/" + matrix + ".mtx";
		}
        std::string vecPath = resDir + "/vectors/" + matrix + ".vec";
        std::cout << "matrix path: " << mtxPath << "\n";
        std::cout << "vector path: " << vecPath << "\n";

        //standard Gauss-Seidel results
        std::unordered_map<std::string, std::any> gsRes;
        gsRes["time"] = 0.0;
        gsRes["iterations"] = 0.0;
        gsRes["clock cycles"] = 0.0;
        gsRes["frequency"] = 0.0;

        //parallel Gauss-Seidel results
        std::vector<std::unordered_map<std::string, std::any>> parallelGsRes(3, std::unordered_map<std::string, std::any>());
        parallelGsRes[0]["name"] = std::string("graph coloring");
        parallelGsRes[1]["name"] = std::string("block coloring");
        parallelGsRes[2]["name"] = std::string("dependency graph");

        parallelGsRes[1]["block size"] = uint32_t(0);

        for(auto &res : parallelGsRes) {

            for(int numThreads = 1; numThreads <= maxThreads; numThreads *= 2) {

                auto threadRes = std::make_shared<std::unordered_map<std::string, std::any>>();
                (*threadRes)["time"]                = 0.0;
                (*threadRes)["clock cycles"]        = 0.0;
                (*threadRes)["iterations"]          = 0.0;
                (*threadRes)["speedup"]             = 0.0;
                (*threadRes)["theoretical speedup"] = 0.0;
                (*threadRes)["overhead"]            = 0.0;
                (*threadRes)["frequency"]           = 0.0;

                std::stringstream ss;
                ss << numThreads << " threads";
                std::string threadString = ss.str();
                res[threadString] = threadRes;
            }
        }

        for(int n = 0; n < testsPerMatrix; n++)
            run_matrix_test(mtxPath, vecPath, gsRes, parallelGsRes[0], parallelGsRes[1], parallelGsRes[2], maxThreads);

        for(const auto &entry : gsRes)
            gsRes[entry.first] = std::any_cast<double>(gsRes[entry.first]) / (double) testsPerMatrix;

        for(auto &res : parallelGsRes)
            for(int numThreads = 1; numThreads <= maxThreads; numThreads *= 2) {

                std::stringstream ss;
                ss << numThreads << " threads";
                std::string threadString = ss.str();
                auto threadRes = std::any_cast<std::shared_ptr<std::unordered_map<std::string, std::any>>>(res[threadString]);

                for(const auto &entry : (*threadRes))
                	(*threadRes)[entry.first] = std::any_cast<double>((*threadRes)[entry.first]) / (double) testsPerMatrix;
            }

        std::cout << "\nStandard Gauss-Seidel:\n";
        std::cout << "Average time:         " << std::any_cast<double>(gsRes["time"]) << " s\n";
        std::cout << "Average clock cycles: " << std::any_cast<double>(gsRes["clock cycles"]) << "\n";
        std::cout << "Average iterations:   " << std::any_cast<double>(gsRes["iterations"]) << "\n";
        std::cout << "Average frequency:    " << std::any_cast<double>(gsRes["frequency"]) << " MHz\n\n";

        for(auto &res : parallelGsRes) {

            auto name = std::any_cast<std::string>(res["name"]);
            std::stringstream blockSizeSS;
            if(name == "block coloring")
                blockSizeSS << " (block size = " << std::any_cast<uint32_t>(res["block size"]) << ")";
            std::cout << "Parallel Gauss-Seidel with " << name << blockSizeSS.str() << ":\n\n";

            for(int numThreads = 1; numThreads <= maxThreads; numThreads *= 2) {

                std::stringstream ss;
                ss << numThreads << " threads";
                std::string threadString = ss.str();
                auto threadRes = std::any_cast<std::shared_ptr<std::unordered_map<std::string, std::any>>>(res[threadString]);

                std::cout << threadString << ":\n";
                std::cout << "Average time:         " << std::any_cast<double>((*threadRes)["time"]) << " s\n";
                std::cout << "Average clock cycles: " << std::any_cast<double>((*threadRes)["clock cycles"]) << "\n";
                std::cout << "Average iterations:   " << std::any_cast<double>((*threadRes)["iterations"]) << "\n";
                std::cout << "Average speedup:      " << std::any_cast<double>((*threadRes)["speedup"]) << "\n";
                std::cout << "Theoretical speedup:  " << std::any_cast<double>((*threadRes)["theoretical speedup"]) << "\n";
                std::cout << "Overhead:             " << std::any_cast<double>((*threadRes)["overhead"]) << "\n";
                std::cout << "Average frequency:    " << std::any_cast<double>((*threadRes)["frequency"]) << " MHz\n\n";
            }
            
            fflush(stdout);
        }
    }
    return 0;
}
