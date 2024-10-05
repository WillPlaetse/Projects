/* 8/31/98 Mary Elaine Califf
 * a C++ array implementation of queues
 */
// Modifications:
//    8/20/2003 Mary Elaine Califf
//        Changed to ANSI C++
// Revised 12/25/2020
// __Will Verplaetse___ and Mary Elaine Califf S2021
//Implementation of a queue of CustomerData objects

#include <iostream>
#include "Queue.h"

using namespace std;

Queue::Queue() // constructor
{
  size = 0;
  head = nullptr;
  tail = nullptr;
}

bool Queue::enqueue(CustomerData newValue) // add newValue to queue
{

  if(size == 0)
  {

    
    this->head = new Node(nullptr, newValue);
    this->tail = this->head;
    size++;
    return true;

  }
  else
  {
    this->tail->next = new Node(nullptr, newValue);
    this->tail = tail->next;
    size++;
    return true;
  }
  

}

CustomerData Queue::dequeue()
{

  
    Node * temp = head;
    CustomerData tempCust(this->head->cust);
    this->head = this->head->next;
    delete temp;
    temp = nullptr;
    size--;
    return tempCust;

}

Queue& Queue::operator=(const Queue& rhs)
{
  if(this != &rhs)
  {
    clear();
    copy(rhs);
  }
  return *this;
}

void Queue::clear()
{
  

    while(head)
    {
      Node *temp = head;
      this->head = head->next;
      delete temp;
      temp = head;
    }
    head = nullptr;
    tail = nullptr;
    return;
  }


//Copies the 
Queue& Queue::copy(const Queue& orig)
{
    this->size = orig.size;
    if(orig.head)
    {
      this->head =  new Node(nullptr, orig.head->cust);
      this->tail = this->head;
      Node* temp = orig.head->next;
      while(temp)
      {
        this->tail->next = new Node(nullptr, temp->cust);
        temp = temp->next;
        tail = tail->next;
      }
    }
    return *this;


}  

Queue::~Queue()
{
  clear();
}

Queue::Queue(const Queue& orig)
{
  this->copy(orig);
}

