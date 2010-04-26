
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  float x;
  float y;
} val;

typedef struct {
  
  char section[20];
  
  int valueCount;
  val values[4];

////////  node * parent;
  node *child1;
  node *child2;
  node *child3;
  node *child4;
  
} node;








float x_min, x_max, y_min, y_max;

node *HEAD;




void add_value_to_tree(int x,int y)
{
  val newVal;
  newVal.x = x;
  newVal.y = y;
  
  
  
}

void add_node_to_node()
{
  
}




void make_qtree(float *xVect, float *yVect, int n)
{
  
  int i;
  
  
  if(n > 0)
  {
    struct newNode;
    
    
  }
  
  for(i =1; i < n; i++)
  {
    struct newNode;
    
    
    
    
    
    
    
    
    
  }
  
}























http://www.cs.berkeley.edu/~demmel/cs267/lecture26/lecture26.html

Given a list of particle positions, this quadtree can be constructed recursively as follows:

   procedure QuadtreeBuild
     Quadtree = {empty}
       For i = 1 to n          ... loop over all particles
         QuadInsert(i, root)   ... insert particle i in quadtree
       end for
       
       ... at this point, the quadtree may have some empty 
       ... leaves, whose siblings are not empty
       Traverse the tree (via, say, breadth first search), 
         eliminating empty leaves

   procedure QuadInsert(i,n)   
     ... Try to insert particle i at node n in quadtree
     ... By construction, each leaf will contain either 
     ... 1 or 0 particles
     
     if the subtree rooted at n contains more than 1 particle
        determine which child c of node n particle i lies in
          QuadInsert(i,c)
     
     else if the subtree rooted at n contains one particle 
        ... n is a leaf
        add n's four children to the Quadtree
        move the particle already in n into the child 
           in which it lies
        let c be child in which particle i lies
        QuadInsert(i,c)
     
     else if the subtree rooted at n is empty        
        ... n is a leaf 
        store particle i in node n
     
     endif
     






