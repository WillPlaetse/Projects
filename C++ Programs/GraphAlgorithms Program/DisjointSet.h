//Written by Will Verplaetse

#ifndef DISJOINTSET_H
#define DISJOINTSET_H

#include <vector>
#include <iostream>



class DisjoinSet
{

    public: 

        //Creates a Disjoint set of size numObjects
        DisjoinSet(int numObjects);

        //Returns the index of the root of objectIndex
        int find(int objectIndex);

        //Unions objIndex1 and objIndex2 and returns true if all objects are unioned togther
        // and false otherwise
        //Precondition: The indexes must not already be in the same set
        bool doUnion(int objIndex1, int objIndex2);

        //Prints the values to a file
        void printArrayValues(std::ostream &outputStream);


    private:

        std::vector<int> theArray;

        int numValues





};

#endif
