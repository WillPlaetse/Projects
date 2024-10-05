// 2/1/24 Will Verplaetse
#include "Register.h"
#include <iostream>
using namespace std;



//Default Constructor
Register::Register()
{
    this->open = true;
    endTime = 1000;
}

//Constructor that takes in the register number
Register::Register(int regNum)
{
    this->registerNum = regNum;
    this->open = true;
}


//Adds a customer to a register
void Register::pushCustomer(CustomerData newCustomer, int startTime)
{
    this->curCustomer = newCustomer;
    open = false;
    endTime = startTime + newCustomer.serviceTime;

}

//Removes a customer from a register
CustomerData Register::popCustomer()
{
    open = true;
    endTime = 1000;
    return curCustomer;
}
