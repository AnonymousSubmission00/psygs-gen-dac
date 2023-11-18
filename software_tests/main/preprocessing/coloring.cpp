//contains coloring techniques

#include "../../include/hpcg_matrix.hpp"
#include "../../include/IntVector.hpp"
#include "../../include/IntVectorArray.hpp"

struct Node_STRUCT {
  int value;
  Node_STRUCT * next;
};
typedef struct Node_STRUCT Node;

//all methods store in node_colors an IntVector that at index i has the color of row i
//in color_nodes an array of IntVectors that for each color i has an IntVector with all components with that color
//in num_colors the number of colors
//rows with the same color can be computed in parallel, rows with different colors have to be executed in the color order
inline void computeDepCol(HPCGMatrix & A, std::vector<std::vector<int>> & dependencies_column);
inline void ComputeColorNodes(IntVector & node_colors, IntVector color_nodes[], std::vector<int> num_color_nodes, int num_colors);

//creates a vector dependencies_column which stores for all rows i all j for which aji is nonzero in order
inline void computeDepCol(HPCGMatrix & A, std::vector<std::vector<int>> & dependencies_column) {
    for(int i=0; i<A.numberOfRows; i++) {
        for(int j=0; j<A.nonzerosInRow[i]; j++) {
            dependencies_column[A.mtxInd[i][j]].push_back(i);
        }
    }
}

//computes color_nodes from node_colors
inline void computeColorNodes(IntVector & node_colors, IntVectorArray & color_nodes, std::vector<int> num_color_nodes, int num_colors) {
    InitializeIntVectorArr(color_nodes, num_colors);
    std::vector<int> cur_color_nodes(num_colors);
    for(int i=0; i<num_colors; i++) {
        InitializeIntVector(color_nodes.values[i], num_color_nodes[i]);
        cur_color_nodes[i] = 0;
    }
    for(int i=0; i<node_colors.length; i++) {
        color_nodes.values[node_colors.values[i]].values[cur_color_nodes[node_colors.values[i]]] = i;
        cur_color_nodes[node_colors.values[i]]++;
    }
}

//only direct dependencies considered, greedy graph coloring: each node is assigned the first available color that is not used by one of its neighbours
inline void computeGraphColoring(HPCGMatrix & A, IntVector & node_colors, IntVectorArray & color_nodes) {
    InitializeIntVector(node_colors, A.numberOfColumns);

    std::vector<std::vector<int>> dependencies_column(node_colors.length);
    computeDepCol(A, dependencies_column);
    std::vector<int> num_color_nodes(node_colors.length);

    //considering all nonzeros in row and column as dependencies (edges)
    std::vector<bool> allowedColors(node_colors.length);
    //marking all colors as available
    //setting all colors to have 0 components assigned to them
    for(int i=0; i<node_colors.length; i++) {
        allowedColors[i] = true;
        num_color_nodes[i] = 0;
    }

    //coloring
    for(int i=0; i<A.numberOfRows; i++) {
        //dependencies by row
        for(int j=0; j<A.nonzerosInRow[i]; j++) {
            if(A.mtxInd[i][j] >= i) {
                break;
            }
            allowedColors[node_colors.values[A.mtxInd[i][j]]] = false;
        }
        //dependencies by column
        for(int j=0; j<dependencies_column[i].size(); j++) {
            if(dependencies_column[i][j] >= i) {
                break;
            }
            allowedColors[node_colors.values[dependencies_column[i][j]]] = false;
        }
        //assigning the color
        node_colors.values[i] = -1;
        for(int j=0; j<i; j++) {
            if(allowedColors[j]) {
                node_colors.values[i] = j;
                num_color_nodes[j]++;
                break;
            }
        }
        if(node_colors.values[i] == -1) {
            node_colors.values[i] = i;
            num_color_nodes[i]++;
        }
        //marking previous not allowed colors as allowed for next iteration
        for(int j=0; j<A.nonzerosInRow[i]; j++) {
            if(A.mtxInd[i][j] >= i) {
                break;
            }
            allowedColors[node_colors.values[A.mtxInd[i][j]]] = true;
        }
        for(int j=0; j<dependencies_column[i].size(); j++) {
            if(dependencies_column[i][j] >= i) {
                break;
            }
            allowedColors[node_colors.values[dependencies_column[i][j]]] = true;
        }
    }

    //calculating the number of colors
    int maxValue=0;
    for(int i=0; i<node_colors.length; i++) {
        if(node_colors.values[i] > maxValue) {
            maxValue = node_colors.values[i];
        }
    }

    computeColorNodes(node_colors, color_nodes, num_color_nodes, maxValue + 1);
}


//each component is assigned color 0 if it has no dependencies and 1 color larger that its largest neighbour if it does
inline void computeDepGraph(HPCGMatrix & A, IntVector & node_colors, IntVectorArray & color_nodes) {
    InitializeIntVector(node_colors, A.numberOfColumns);
    std::vector<std::vector<int>> dependencies_column(node_colors.length);
    computeDepCol(A, dependencies_column);
    
    //setting all colors to have 0 components assigned to them
    std::vector<int> num_color_nodes(node_colors.length);
    for(int i=0; i<node_colors.length; i++) {
        num_color_nodes[i] = 0;
    }

    //asssigning colors
    int min_color;
    for(int i=0; i<node_colors.length; i++) {
        min_color = -1;
        for(int j=0; j<A.nonzerosInRow[i]; j++) {
            if(A.mtxInd[i][j] >= i) {
                break;
            }
            if(node_colors.values[A.mtxInd[i][j]] > min_color) {
                min_color = node_colors.values[A.mtxInd[i][j]];
            }
        }
        for(int j=0; j<dependencies_column[i].size(); j++) {
            if(dependencies_column[i][j] >= i) {
                break;
            }
            if(node_colors.values[dependencies_column[i][j]] > min_color) {
                min_color = node_colors.values[dependencies_column[i][j]];
            }
        }
        node_colors.values[i] = min_color + 1;
        num_color_nodes[min_color+1]++;
    }

    //calculating the number of colors
    int maxValue=0;
    for(int i=0; i<node_colors.length; i++) {
        if(node_colors.values[i] > maxValue) {
            maxValue = node_colors.values[i];
        }
    }

    computeColorNodes(node_colors, color_nodes, num_color_nodes, maxValue + 1);
}

//computes block coloring with sizes block_size
//each block starts at a component with index a multiple of block_size and ends at the following one or at the end of all components
//rows in each block are computed sequentially, each block has a color
//rules for order between different blocks are the same as coloring techniques
inline void computeBlockColoring(HPCGMatrix & A, int block_size, IntVector & block_colors, IntVectorArray & color_blocks) {
    //compute the number of blocks
    int num_blocks = A.numberOfColumns/block_size;
    InitializeIntVector(block_colors, num_blocks);

    //compute dependencies between blocks in rows and columns
    std::vector<Node*> dependencies(block_colors.length);
    for(int i=0; i<num_blocks; i++) {
        dependencies[i] = NULL;
    }
    Node* n;
    Node* currNode;
    for(int i=0; i<num_blocks*block_size; i++) {
        for(int j=0; j<A.nonzerosInRow[i]; j++) {   
            if(A.mtxInd[i][j] >= num_blocks*block_size) {
                break;
            }       
            //insert first dependency
            n = (Node*) malloc(sizeof(Node));
            n -> value = A.mtxInd[i][j]/block_size;
            currNode = dependencies[i/block_size];
            if(currNode == NULL || currNode -> value > n -> value) {
                n -> next = currNode;
                dependencies[i/block_size] = n;
            } else {
                while(currNode -> next != NULL && currNode -> next -> value < n -> value) {
                    currNode = currNode -> next;
                }
                if(currNode -> next == NULL || currNode -> next -> value != n -> value) {
                    n -> next = currNode -> next;
                    currNode -> next = n;    
                } else {
                    free(n);
                }          
            }
            //insert second dependency
            n = (Node*) malloc(sizeof(Node));
            n -> value = i/block_size;
            currNode = dependencies[A.mtxInd[i][j]/block_size];
            if(currNode == NULL || currNode -> value > n -> value) {
                n -> next = currNode;
                dependencies[A.mtxInd[i][j]/block_size] = n;
            } else {
                while(currNode -> next != NULL && currNode -> next -> value < n -> value) {
                    currNode = currNode -> next;
                }
                if(currNode -> next == NULL || currNode -> next -> value != n -> value) {
                    n -> next = currNode -> next;
                    currNode -> next = n;    
                } else {
                    free(n);
                }          
            }
        }
    }

    //compute coloring based on dependencies
    std::vector<int> num_color_blocks(block_colors.length);
    std::vector<bool> allowedColors(block_colors.length);
    //marking all colors as available
    //setting all colors to have 0 components assigned to them
    for(int i=0; i<block_colors.length; i++) {
        allowedColors[i] = true;
        num_color_blocks[i] = 0;
    }
    //coloring
    for(int i=0; i<num_blocks; i++) {
        currNode = dependencies[i];
        while(currNode != NULL) {
            if(currNode -> value >= i) {
                break;
            }
            allowedColors[block_colors.values[currNode->value]] = false;
            currNode = currNode->next;
        }
        //assigning the color
        block_colors.values[i] = -1;
        for(int j=0; j<i; j++) {
            if(allowedColors[j]) {
                block_colors.values[i] = j;
                num_color_blocks[j]++;
                break;
            }
        }
        if(block_colors.values[i] == -1) {
            block_colors.values[i] = i;
            num_color_blocks[i]++;
        }
        //marking previous not allowed colors as allowed for next iteration
        currNode = dependencies[i];
        while(currNode != NULL) {
            if(currNode -> value >= i) {
                break;
            }
            allowedColors[block_colors.values[currNode->value]] = true;
            currNode = currNode->next;
        }
    }

    //calculating the number of colors
    int maxValue=0;
    for(int i=0; i<block_colors.length; i++) {
        if(block_colors.values[i] > maxValue) {
            maxValue = block_colors.values[i];
        }
    }

    computeColorNodes(block_colors, color_blocks, num_color_blocks, maxValue + 1);

    for(int i=0; i<num_blocks; i++) {
        Node* currNode = dependencies[i];
        Node* nextNode;
        while(currNode != NULL) {
            nextNode = currNode -> next;
            free(currNode);
            currNode = nextNode;
        }
    }
}
