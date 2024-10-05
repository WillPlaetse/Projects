/* 8/31/98 Mary Elaine Califf
 * a C++ array implementation of a queue of integers which may have length of up to 100
 *
 * Revised 12/25/2020
 */
 // _Will Verplaetse____ and Mary Elaine Califf S2021
 //Header file for a queue of CustomerData objects

#ifndef QUEUE_H
#define QUEUE_H
#include "CustomerData.h"

class Queue
{
public:

  // constructor
  Queue();


  // getter for size
  int getSize() { return size; }

  // is the queue empty
  bool isEmpty() { return (size == 0); }

  // adds a new value to the queue
  // fails and returns false if the queue is full
  bool enqueue(CustomerData newValue);

  // removes a value from the queue and returns it
  // Precondition: queue must not be empty
  CustomerData dequeue();

  // return item at the front of the queue without removing it
  // Precondtion: queue must not be empty
  CustomerData front() { return head->cust; }

  //Assignment operator
  Queue& operator=(const Queue& rhs);

  //Destructor
  ~Queue();

  //Copy Constructor
  Queue(const Queue& orig);
  
private:
  int size;

  struct Node
  {
    Node* next;
    CustomerData cust;

    Node(){}
    Node(Node* nextPtr , CustomerData customer) : cust(customer), next(nextPtr) {}

  };

  Node * head;
  Node * tail;

  //Helper method for copy constructor and assignment operator
  Queue& copy(const Queue& orig);

  //Helper method for destructor and assignment operator
  void clear();

};

#endif
