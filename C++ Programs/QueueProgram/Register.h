//2/1/24
// Will Verplaetse

#ifndef REGISTER_H
#define REGISTER_H
#include "CustomerData.h"

class Register
{

    public:

    //Default Constructor
    Register();

    //Constructor that takes an int parameter
    Register(int regNum);

    //Returns register number
    int getRegisterNum(){ return registerNum;}

    //Adds a customer to the register
    void pushCustomer(CustomerData newCustomer, int startTime);

    //Removes current customer from register 
    CustomerData popCustomer();

    //Returns whether the register is open or not
    bool isOpen(){return open;}

    //Returns customer end time
    int getEndTime(){return endTime;}

    //Sets register Num
    void setRegisterNum(int num){registerNum = num; }

    //Returns the customer's number
    int getCustomerNum(){ return curCustomer.customerNumber;}


    void setEndTime(int time){ endTime = time;}



    private:
    CustomerData curCustomer;
    int registerNum;
    bool open;
    int endTime;

};

#endif