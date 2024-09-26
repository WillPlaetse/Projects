// DisjointSet implementation using union by size and path compression
// By Mary Elaine Califf and Will Verplaetse

#include "DisjointSet.h"
#include <iostream>


DisjointSet::DisjointSet(int numObjects)
{

    this->numValues = numObjects;   
    for (int i = 0; i < numObjects; i++)
    {
        theArray.push_back(-1);
    }

} 
//recursive method to find the item -- does path compression on the way out of the recursion
int DisjointSet::find(int objectIndex)
{
    if(theArray[objectIndex] < 0)
    {
        return objectIndex;
    }
    else
    {

        int rootIndex = find(theArray[objectIndex]);

        theArray[objectIndex] = rootIndex;

        return rootIndex;

    }
}

bool DisjointSet::doUnion(int objIndex1, int objIndex2)
{
    
    int object1Root = this->find(objIndex1);
    int object2Root = this->find(objIndex2);


    if( theArray[object2Root] < theArray[object1Root])
    {
        theArray[object2Root] += theArray[object1Root];
        theArray[object1Root] = object2Root;

       
        return abs(theArray[object2Root]) == this->numValues;

    }
     else
    {
        
        theArray[object1Root] += theArray[object2Root];
        theArray[object2Root] = object1Root;

        return abs(theArray[object1Root]) == this->numValues;


    }

}

void DisjointSet::printArrayValues(std::ostream &outputStream)
{
    for (int i = 0; i < numValues; i++)
    {
        outputStream << theArray[i] << " ";
    }
    outputStream << std::endl;
}
