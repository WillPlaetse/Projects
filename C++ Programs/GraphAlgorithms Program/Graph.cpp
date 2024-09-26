
//Will Verplaetse

#include "Graph.h"
#include <queue>
#include <fstream>
#include <iostream>


Graph::Graph()
{
    this->numOfVertices = 0;
    this->numOfEdges = 0;

}


bool Graph::readGraph(string filename)
{

    ifstream inFile;
    inFile.open(filename);
    if(inFile.fail())
    {
        return false;
    }
    else
    {


        int vertices = 0;
        inFile>>vertices;
        this->numOfVertices = vertices;

        //Clearing out any previously held data
        lookupTable.clear();
        adjacencyMatrix.clear();

        //Creating the new graph
        for (int i = 0; i < numOfVertices; i++)
            adjacencyMatrix.push_back(vector<int>(numOfVertices, -1));


        string vertexName = "";
        for(int i = 0; i < numOfVertices; i++)
        {
            inFile>>vertexName;
            lookupTable.push_back(vertexName);
        }


        int edges = 0;
        inFile>>edges;
        this->numOfEdges = edges;


        for(int i = 0; i < numOfEdges; i++)
        {

            string fromVertex = "";
            string toVertex = "";
            int weight = 0;

            inFile>>fromVertex;
            inFile>>toVertex;
            inFile>>weight;

            int fromVertexIndex = translateNameToIndex(fromVertex);
            int toVertexIndex = translateNameToIndex(toVertex);

            adjacencyMatrix[fromVertexIndex][toVertexIndex] = weight;


        }

    return true;

    }

}




void Graph::printGraph()
{

    cout<<numOfVertices<<endl;
    for(int i = 0; i < numOfVertices; i++)
    {
        cout<<lookupTable[i]<<endl;
    }

    cout<<numOfEdges<<endl;
    for(int i = 0; i < numOfVertices; i++)
    {

        for(int j = 0; j < numOfVertices; j++)
        {
            if(adjacencyMatrix[i][j] != -1)
            {
                cout<<lookupTable[i]<<" "<<lookupTable[j]<<" "<<adjacencyMatrix[i][j]<<endl;
            }
        }

    }


}


void Graph::computeTopologicalSort()
{

    //Counting in degrees
    int* inDegrees = new int[numOfVertices]{0};
    for(int i = 0; i < numOfVertices; i++)
    {
        for(int j = 0; j < numOfVertices; j++)
        {
            if(adjacencyMatrix[i][j] != -1)
            {
                inDegrees[j]++;
            }
        }
    }

    queue<int> vertexQueue;


    int numIncluded = 0;
    string ordering = "";
    //Starting the queue
    for(int i = 0; i < numOfVertices; i++)
    {
        if(inDegrees[i] == 0)
        {
            vertexQueue.push(i);
        }
    }

    //Main loop
    while(!vertexQueue.empty())
    {
        
        //Marking the vertex found and adding it to the ordering
        numIncluded++;
        ordering = ordering + lookupTable[vertexQueue.front()] + " --> ";

        //Decrementing edges
        for(int i = 0; i < numOfVertices; i++)
        {
            if(adjacencyMatrix[vertexQueue.front()][i] != -1)
            {
                inDegrees[i]--;
                if(inDegrees[i] == 0)
                {
                    vertexQueue.push(i);
                }
            }
        }

        //Pushing vertices with current in degrees of zero
        for(int i = 0; i < numOfVertices; i++)
        {
            if(inDegrees[i] == 0 && !includedArr[i])
            {
                vertexQueue.push(i);
                includedArr[i] = true;
            }
        }

        vertexQueue.pop();


    }

    if(numIncluded == numOfVertices)
    {
        ordering = ordering.substr(0, ordering.length() - 5);
        cout<<"Topological Sort:\n";
        cout<<ordering<<endl;
    }
    else
    {
        cout<<"This graph cannot be topologically sorted."<<endl;
    }

    delete [] inDegrees;

}


//Finds the shortest path from the node passed in to all other vertices if possible and prints the results 
void Graph::computeShortestPath(string nodeName)
{

    int startNodeIndex = translateNameToIndex(nodeName);
    
    bool* foundArray = new bool[numOfVertices]{false};
    foundArray[startNodeIndex] = true;

    bool allFound = false;
    int numFound = 1;


    int* pathCostArray = new int[numOfVertices]{0};

    int pathArray[numOfVertices];
    pathArray[startNodeIndex] = -1;

    std::priority_queue<Edge, std::vector<Edge>, greater<Edge>> minHeap;


    //pushing the start index edges to the priority queue
    for(int i = 0; i < numOfVertices; i++)
    {
        if(adjacencyMatrix[startNodeIndex][i] != -1)
        {
            
            Edge startingEdge(adjacencyMatrix[startNodeIndex][i], startNodeIndex, i);
            minHeap.push(startingEdge);

        }
    }

    //Main loop
    while(!minHeap.empty() && !allFound)
    {

        Edge minEdge(minHeap.top().weight, minHeap.top().fromIndex, minHeap.top().toIndex);
        minHeap.pop();
        if( !foundArray[minEdge.toIndex])
        {
            foundArray[minEdge.toIndex] = true;
            numFound++;
            if(numFound == numOfVertices)
            {
                allFound = true;
            }
            pathCostArray[minEdge.toIndex] = minEdge.weight;
            pathArray[minEdge.toIndex] = minEdge.fromIndex;


            //Pushing the newly found vertex's edges to the queue with the added condition of
            // checking that it isn't adding any edges to vertices that are already found
            for(int i = 0; i < numOfVertices; i++)
            {
                if(adjacencyMatrix[minEdge.toIndex][i] != -1 && !foundArray[i])
                {
                    //Using the weight as path cost instead of individual edge weight in this case
                    Edge anotherEdge( pathCostArray[minEdge.toIndex] + adjacencyMatrix[minEdge.toIndex][i] , minEdge.toIndex, i);
                    minHeap.push(anotherEdge);

                }
            }
        }

        

    }


    cout<<"Shortest paths from "<<nodeName<<":\n";
    for(int i = 0; i < numOfVertices; i++)
    {
        string thePath = "";
        if(i != startNodeIndex && foundArray[i])
        {

            thePath = findShortestPath(i, startNodeIndex, pathArray);
            cout<<thePath<<" || Weight: "<<pathCostArray[i]<<endl;
        }
        else if(!foundArray[i])
        {
            thePath = "No path from " + lookupTable[startNodeIndex] + " to " + lookupTable[i] + " found.";
            cout<<thePath<<endl;
        }

        thePath = "";

    }

    delete [] foundArray;
    delete [] pathCostArray;

}



void  Graph::computeMinimumSpanningTree()
{
    //Creating the Disjoint set and pushing edges into a vector, then sorting the edges
    DisjointSet* vertexSet = new DisjointSet(numOfVertices);
    std::vector<Edge> listOfEdges;

    
    for(int i = 0; i < numOfVertices; i++)
    {
        for(int j = 0; j < numOfVertices; j++)
        {
            if(adjacencyMatrix[i][j] != -1)
            {
                Edge anEdge(adjacencyMatrix[i][j], i, j);
                listOfEdges.push_back(anEdge);
            }
        }
    }

    std::sort(listOfEdges.begin(), listOfEdges.end());


    //Create a vector of the indices of the Edges used
    bool* usedEdges = new bool[numOfEdges]{false};
    int totalCost = 0;

    bool allVerticesIncluded = false;

    //Unioning until edges run out or the minimum spanning tree has been found
    for(int i = 0; i < numOfEdges && !allVerticesIncluded; i++)
    {
        if(vertexSet->find(listOfEdges[i].fromIndex) != vertexSet->find(listOfEdges[i].toIndex))
        {
            allVerticesIncluded = vertexSet->doUnion(listOfEdges[i].fromIndex, listOfEdges[i].toIndex);
            usedEdges[i] = true;
            totalCost += listOfEdges[i].weight;

        }
    }


    cout<<"Minimum Spanning Tree:\n";
    for(int i = 0; i < numOfEdges; i++)
    {   
        if(usedEdges[i])
        {
            cout<<lookupTable[listOfEdges[i].fromIndex]<<" -- "<<lookupTable[listOfEdges[i].toIndex]
        <<" || Weight: "<<listOfEdges[i].weight<<endl;
        }

    }

    cout<<"Total Cost: "<<totalCost<<endl;

    delete vertexSet;
    delete [] usedEdges;

}


//Translates a name to a index in the adjacency matrix
int Graph::translateNameToIndex(string nodeName)
{

    for(int i = 0; i < numOfVertices; i++)
    {
        if(nodeName == lookupTable[i])
        {
            return i;
        }
    }

    return -1;

}

//Helper method for computeShortestPath
string Graph::findShortestPath(int index, int rootIndex, int pathArray[])
{

    if(index == rootIndex)
    {
        return lookupTable[index];
    }
    else
    {
        return findShortestPath(pathArray[index], rootIndex, pathArray) + " --> " + lookupTable[index];
    }

}



bool Graph::Edge::operator>(const Edge& edge2) const
    {
        if(weight < edge2.weight)
        {
            return false;
        }
        else if(weight == edge2.weight)
        {
            if(fromIndex < edge2.fromIndex)
            {
                return false;
            }
            else if(fromIndex == edge2.fromIndex)
            {
            
                if(toIndex < edge2.toIndex)
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }
            else
            {
                return true;
            } 
        }
        else
        {
            return true;
        }
    }



bool Graph::Edge::operator<(const Edge& edge2) const
    {
        if(weight < edge2.weight)
        {
            return true;
        }
        else if(weight == edge2.weight)
        {
            if(fromIndex < edge2.fromIndex)
            {
                return true;
            }
            else if(fromIndex == edge2.fromIndex)
            {
            
                if(toIndex < edge2.toIndex)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                return false;
            } 
        }
        else
        {
            return false;
        }


    }