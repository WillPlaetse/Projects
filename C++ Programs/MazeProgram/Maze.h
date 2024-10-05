// Class to generate a random maze using disjoint sets and print it in ASCII format to a file
// By Mary Elaine Califf and Will Verplaetse
#ifndef MAZE_H
#define MAZE_H

#include <iostream>
#include <fstream>
using namespace std;

struct CellWalls
{
    bool south;
    bool east;
    CellWalls(bool startEast = true, bool startSouth = true) : east(startEast), south(startSouth) {}
};

class Maze
{
private:
    CellWalls *mazeWalls; // maze cells
    int numRows;
    int numColumns;

public:
    // constructor
    Maze(int rows, int cols);

    // copy constructor
    Maze(const Maze &orig) { copy(orig); };

    // destructor
    ~Maze() { delete [] mazeWalls; }

    // assignment operator
    Maze &operator=(const Maze &rhs);

    // generates the maze by randomly knocking down walls
    void generateMaze(bool stopEarly);

    // prints the maze to the specified outputStream
    void print(ostream &outputStream);



private:
    // private helper method to copy the data from another Maze object
    void copy(const Maze &orig);


    //Checks to make sure an edge wall isn't being selected for knockdown
    //Returns true if an edge wall is being selected for knockdown and false otherwise
    bool isAnEdgeWall(int startCell, int direction);


    //Returns the index of the neighbor of the startIndex
    int findNeighbor(int startIndex, int direction);


    //Knocks down a wall
    void knockdownWall(int startIndex, int neighborIndex, int direction);

};

#endif
