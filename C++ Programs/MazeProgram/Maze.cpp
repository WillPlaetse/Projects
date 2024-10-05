// Implementation of Maze class
// By Mary Elaine Califf and Will Verplaetse

#include "Maze.h"
#include "DisjointSet.h"
using namespace std;

Maze::Maze(int rows, int cols)
{
    numRows = rows;
    numColumns = cols;
    int numCells = rows * cols;
    mazeWalls = new CellWalls[numCells];
    mazeWalls[numCells - 1].east = false;
}

Maze &Maze::operator=(const Maze &rhs)
{
    if (this != &rhs)
    {
        delete [] mazeWalls;
        this->copy(rhs);
    }
    return *this;
}

void Maze::generateMaze(bool stopEarly)
{
    int numCells = numRows * numColumns;
    DisjointSet mySet(numCells);
    bool mazeComplete = false;

    int startIndex = 0;
    int direction = 0;
    int neighborIndex = 0;


    while(!mazeComplete)
    {
        
        startIndex = (rand() % numCells);
        direction = rand() % 4;

        if(isAnEdgeWall(startIndex, direction))
        {
            direction = (direction + 2) % 4;
        }

        neighborIndex = findNeighbor(startIndex, direction);



        if(mySet.find(startIndex) != mySet.find(neighborIndex))
        {
  
            knockdownWall(startIndex, neighborIndex, direction);
            mazeComplete = mySet.doUnion(startIndex, neighborIndex); 
            mazeComplete =  mySet.find(0) == mySet.find(numCells -1);

        }



    }


}

void Maze::print(ostream &outputStream)
{
    // print the top row of walls
    for (int i = 0; i < numColumns; i++)
        outputStream << " _";
    outputStream << '\n';
    for (int i = 0; i < numRows; i++)
    {
        int cellbase = i * numColumns;
        // print west wall (except at entrance)
        if (i == 0)
            outputStream << ' ';
        else
            outputStream << '|';
        for (int j = 0; j < numColumns; j++)
        {
            if (mazeWalls[cellbase + j].south)
                outputStream << '_';
            else
                outputStream << ' ';
            if (mazeWalls[cellbase + j].east)
                outputStream << '|';
            else
                outputStream << ' ';
        }
        outputStream << '\n';
    }
}

void Maze::copy(const Maze &orig)
{
    this->numRows = orig.numRows;
    this->numColumns = orig.numColumns;
    int numCells = numRows * numColumns;
    mazeWalls = new CellWalls[numCells];
    for (int i = 0; i < numCells; i++)
    {
        this->mazeWalls[i] = orig.mazeWalls[i];
    }
}

//Checks if the cell is trying to knockdown an edge wall
bool Maze::isAnEdgeWall(int startCell, int direction)
{
   
    //Going up
    if((startCell < numColumns) && direction == 3)
    {
        return true;
    }
    //Going down
    else if(startCell >= ((numColumns * numRows) - numColumns)  &&  direction == 1  )
    {
        return true;
    }
    //going left 
    else if ( (startCell % numColumns == 0) && direction == 0)
    {
        return true;
    }
    //going right
    else if( (startCell % numColumns == numColumns -1 ) && direction == 2)
    {
        return true; 
    }
    else
    {
        return false;
    }
}

//Finds the neighbor
int Maze::findNeighbor(int startIndex, int direction)
{
    //going left
    if(direction == 0)
    {
        return startIndex - 1;
    }
    //going down
    else if(direction == 1)
    {
        return startIndex + numColumns;
    }
    //going right
    else if (direction == 2)
    {
        return startIndex + 1;
    }
    //going up
    else 
    {
        return startIndex - numColumns;
    }


}

//Knocks down the appropriate wall
void Maze::knockdownWall(int startIndex, int neighborIndex, int direction)
{
    if(direction == 0)
    {
         this->mazeWalls[neighborIndex].east = false;
    }
    else if(direction == 1)
    {
        this->mazeWalls[startIndex].south = false;
    }
    else if( direction == 2)
    {
        this->mazeWalls[startIndex].east = false;
    }
    else
    {
        this->mazeWalls[neighborIndex].south = false;
    }
}
