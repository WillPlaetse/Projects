// Simulation to demonstrate that one queue feeding multiple cash registers is more efficient than have one queue per cash register
// __Will Verplaetse__ (don't forget to put your name on the file)

#include <iostream>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include "Register.h"
#include "Queue.h"
using namespace std;


//finds the smallest queue and returns that queue
Queue& findSmallestLine(Queue multiQueues[], Register regs[], int time);


//check to see if there is an open register for the single line scenario
bool areSingleRegistersOpen(Register regs[]);

// returns the correct open register 
Register& findOpenRegister(Register regs[]);

//Removes finished customers from their registers
void checkForFinishedSingleRegisters(Register regs [], int time, ofstream & singleFile, ofstream & multiFile);


// Moves the front customer to the appropriate Register and prints the appropriate statement
// as well as keep track of average wait time and max wait time
void moveSingleQueueAlong(Register regs [], ofstream & singleFile, Queue & oneQueue, int time, int& maxWaitTime, int& avgWaitTime);

// Moves the front customer to the appropriate Register and prints the appropriate statement
// as well as keep track of average wait time and max wait time
// This method needs a queueNum however to get the right register in the register array
void moveMultiQueueAlong(Register regs[], ofstream & multiFile, Queue & theQueue, int time, int queueNum, int& maxWaitTime, int& avgWaitTime);


// Adds a new customer to the appropriate queue and prints the appropriate message to the console
void queueNewCustomer(CustomerData aCustomer, int maxServiceTime, Queue & oneQueue, ofstream & outFile);


int main(int argc, char** argv)
{
    // code to get the command line arguments
    if (argc < 4)
    {
        cout << "Usage: " << argv[0] << " maxTimeBetweenCustomers maxServiceTime randomSeed" << endl;
        return 1;
    }
    int maxTimeBetweenCustomers = atoi(argv[1]);
    int maxServiceTime = atoi(argv[2]);
    int randomSeed = atoi(argv[3]);
    if (randomSeed > 0)
    {
        srand(randomSeed);
    }
    else
    {
        srand(time(nullptr));
    }

    // if doing extra credit -- need to handle additional OPTIONAL command line argument


    

    struct records
    {
        int avgWaitCount = 0;
        int maxWaitTime = 0;
        int maxLineLength = 0;
    }singleRecs, multiRecs;

    


    //queue for the one line scenario
    Queue oneQueue;



    //queues for the multi line scenario
    Queue multiQueues[3];


    //sets up registers for both scenarios
    Register regs[6];
    regs[0].setRegisterNum(1);
    regs[1].setRegisterNum(2);
    regs[2].setRegisterNum(3);
    regs[3].setRegisterNum(1);
    regs[4].setRegisterNum(2);
    regs[5].setRegisterNum(3);
    
    
    //creates the first customer and the arrival time variable as
    // well as the number of customers
    int totalCustomers = 1;
    int arrTime = rand() % maxTimeBetweenCustomers + 1;

    
    int time = arrTime;

   
    
    //Creating the files
    ofstream singleFile;
    singleFile.open("singleline.txt");
    ofstream multiFile;
    multiFile.open("multiplelines.txt");




    for(; time <= 720 ; time++)
    {
        

        
        checkForFinishedSingleRegisters(regs, time, singleFile, multiFile);

        moveSingleQueueAlong(regs, singleFile, oneQueue, time, singleRecs.maxWaitTime, singleRecs.avgWaitCount);

        for(int i = 0; i < 3; i++)
        {
        moveMultiQueueAlong(regs, multiFile, multiQueues[i], time, i, multiRecs.maxWaitTime, multiRecs.avgWaitCount);
        }






        if( time == arrTime)
        {
            CustomerData aCustomer(totalCustomers, time, rand() % maxServiceTime + 1);
            
            
            totalCustomers++;

            queueNewCustomer(aCustomer, maxServiceTime, oneQueue, singleFile);


            queueNewCustomer(aCustomer, maxServiceTime, findSmallestLine(multiQueues, regs, time) , multiFile);

            arrTime = (rand() % maxTimeBetweenCustomers + 1) + arrTime;
        }


        //Calling these functions because the queue customer methods only add the customer to the queue
        //They do not push customers to a register which is what the move line along methods do
        moveSingleQueueAlong(regs, singleFile, oneQueue, time, singleRecs.maxWaitTime, singleRecs.avgWaitCount);

       for(int i = 0; i < 3; i++)
        {
        moveMultiQueueAlong(regs, multiFile, multiQueues[i], time, i, multiRecs.maxWaitTime, multiRecs.avgWaitCount);
        }





        if(singleRecs.maxLineLength < oneQueue.getSize())
        {
            singleRecs.maxLineLength = oneQueue.getSize();
        }

        if(multiRecs.maxLineLength < multiQueues[0].getSize() + multiQueues[1].getSize() + multiQueues[2].getSize())
        {
            multiRecs.maxLineLength = multiQueues[0].getSize() + multiQueues[1].getSize() + multiQueues[2].getSize();
        }







    }

    //subtracts the number of customers who were still in the queue
    totalCustomers = totalCustomers - oneQueue.getSize() - 1 ;
    double singleAvg = singleRecs.avgWaitCount / (double) (totalCustomers);
 
    totalCustomers += oneQueue.getSize();

    totalCustomers = totalCustomers - (multiQueues[0].getSize() + multiQueues[1].getSize() + multiQueues[2].getSize());

    double multiAvg = multiRecs.avgWaitCount / (double) totalCustomers;

    singleFile<<""<<endl;

    singleFile<<fixed;
    singleFile<<setprecision(1);
    singleFile<<"Average wait time: "<<setprecision(1)<<singleAvg<<" minutes"<<endl;
    singleFile<<"Maximum wait time: "<<singleRecs.maxWaitTime<<" minutes"<<endl;
    singleFile<<"Maximum line length: "<<singleRecs.maxLineLength<<" customers"<<endl;


    multiFile<<""<<endl;

    multiFile<<fixed;
    multiFile<<"Average wait time: "<<setprecision(1)<<multiAvg<<" minutes"<<endl;
    multiFile<<"Maximum wait time: "<<multiRecs.maxWaitTime<<" minutes"<<endl;
    multiFile<<"Maximum line length: "<<multiRecs.maxLineLength<<" customers"<<endl;


    singleFile.close();
    multiFile.close();

    return 0;
}




//finds the line with the least amount of people. 
Queue& findSmallestLine(Queue multiQueues[], Register regs[], int time)
{

    int q1Size = multiQueues[0].getSize();
    int q2Size = multiQueues[1].getSize();
    int q3Size = multiQueues[2].getSize();
    if( !(regs[3].isOpen()))
    {
        q1Size++;
    }
    if( !(regs[4].isOpen()))
    {
        q2Size++;
    }
    if( !(regs[5].isOpen()))
    {
        q3Size++;
    }



    if( q1Size <= q2Size && q1Size <= q3Size )
    {
        
        return multiQueues[0];
    }
    else if(q2Size < q1Size && q2Size <= q3Size)
    {

            return multiQueues[1];

    }
    else
    {
        return multiQueues[2];
    }

}



//check to see if there is an open register for the single line scenario
bool areSingleRegistersOpen(Register regs[])
{
    return (regs[0].isOpen() || regs[1].isOpen() || regs[2].isOpen());
}

// returns the correct open register for the single line scenario
Register& findOpenRegister(Register regs[])
{

    if(regs[0].isOpen())
    {
        return regs[0];
    }
    else if(regs[1].isOpen())
    {
        return regs[1];
    }
    else
    {
        return regs[2];
    }


}


//Clears registers if it they are done servicing customers
void checkForFinishedSingleRegisters(Register regs[] , int time, ofstream & singleFile, ofstream & multiFile)
{
    int i = 0;
    for(; i < 3; i++)
    {
        if(regs[i].getEndTime() <= time)
        {
            singleFile<<"Customer "<<regs[i].getCustomerNum()<<" departed at "<<time<<"."<<endl;
            regs[i].popCustomer();
        }
    }

    for(; i < 6; i++)
    {
        if(regs[i].getEndTime() <= time)
        {
            multiFile<<"Customer "<<regs[i].getCustomerNum()<<" departed at "<<time<<"."<<endl;
            regs[i].popCustomer();
        }
    }
}


// adds a new customer to the queue and prints the appropriate message to the file
void queueNewCustomer(CustomerData aCustomer, int maxServiceTime, Queue & theQueue, ofstream & outFile)
{

    outFile<<"Customer "<<aCustomer.customerNumber<<" arrived at "<<aCustomer.arrivalTime<<"."<<endl;
    theQueue.enqueue(aCustomer);
    
}



// moves the single queue along print the correct messages to the file
void moveSingleQueueAlong(Register regs [], ofstream & singleFile, Queue & oneQueue, int time, int& maxWaitTime, int& avgWaitTime)
{
    
    while( !(oneQueue.isEmpty()) && areSingleRegistersOpen(regs) )
    {     
        Register & aReg = findOpenRegister(regs);
        singleFile<<"Customer "<<oneQueue.front().customerNumber<<" served by "<<aReg.getRegisterNum()
            <<" at "<<time<<"."<<endl; 

        CustomerData frontCustomer = oneQueue.dequeue();
        int waitTime = time - frontCustomer.arrivalTime;
        if(maxWaitTime < waitTime)
        {
            maxWaitTime = waitTime;
        }
        avgWaitTime += waitTime;

        aReg.pushCustomer(frontCustomer, time);

  
    }
    
}



//Moves ONE of the multiple queues along. The specific queue is indicated by the queueNum
// Same logic as the single queue version above
void moveMultiQueueAlong(Register regs[], ofstream & multiFile, Queue & theQueue, int time, int queueNum, int& maxWaitTime, int& avgWaitTime)
{
    if(!(theQueue.isEmpty()))
    {    
        queueNum += 3;
        if(regs[queueNum].isOpen())
        {

            multiFile<<"Customer "<<theQueue.front().customerNumber<<" served by "<<regs[queueNum].getRegisterNum()
                <<" at "<<time<<"."<<endl;

            CustomerData frontCustomer = theQueue.dequeue();
            int waitTime = time - frontCustomer.arrivalTime;
            if(maxWaitTime < waitTime)
            {
                maxWaitTime = waitTime;
            }
            avgWaitTime += waitTime;
            regs[queueNum].pushCustomer(frontCustomer, time); 
        }

    }
}

