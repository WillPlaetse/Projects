//Will Verplaetse

#ifndef GRAPH_H
#define GRAPH_H

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include "DisjointSet.h"
using namespace std;


class Graph
{

public:

    Graph();

    //Reads in a graph stored in the file filename and constructs the appropriate graph
    bool readGraph(string filename);

    //Prints the graph out in the same format it was read in
    void printGraph();

    //Computes the topological sort of the graph if possible, prints an error message if not possible
    void computeTopologicalSort();

    //Computes the shortest path from vertex nodeName to all other vertices if there is a path
    void computeShortestPath(string nodeName);


    //Computes the minimum spanning tree of the graph. 
    //Precondition: The graph must be connected 
    void computeMinimumSpanningTree();

private:

    //A struct that will be used for computing the shortest path
    struct Edge
    {
    int weight;
    int fromIndex;
    int toIndex;
    Edge(int readWeight, int readFromIndex, int readToIndex) : weight(readWeight), fromIndex(readFromIndex), toIndex(readToIndex) {}

    //Overriden operator that compares Edge structs
    bool operator>(const Edge& edge2) const;
    
    //Overloaded operator that compares Edge structs
    bool operator<(const Edge& edge2) const;
    };

    int numOfVertices;
    int numOfEdges;

    //Vector used to translate names to indices and vice versa
    std::vector<string> lookupTable;

    //An adjacency matrix representation of the graph
    std::vector< std::vector<int> > adjacencyMatrix;
    
    //Translates a name into a index. Returns -1 if the name is not found
    int translateNameToIndex(string nodeName);


    //Returns a string that represents the shortest path between two vertices
    //Used only as helper in computeShortestPath
    string findShortestPath(int index, int rootIndex, int pathArray[]);    



};

#endif