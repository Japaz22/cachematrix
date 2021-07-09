## R-Programming Peer Graded Assignment Coursera week 3.
## This function creates a matrix that is able to store the inverse.
## Im not fit for programming life.


makeCacheMatrix <- function(x = matrix()) {
  nill<-NULL
  set<-function(y){
    x<<-y
    nill<<- NULL
  }
get<-function(){x}
setInverse<-function(inverse){nill<<-inverse}
getInverse<-function(){nill}
list(set=set, 
     get=get, 
     setInverse=setInverse, 
     getInverse=getInverse)

}


## This function calculates and stores the matrix created from the above function.

cacheSolve <- function(x,...) {
  inv<-x$getInverse()
  if(!is.null(nill)){
    message("getting cached data")
    return(nill)
  }
  mat<- x$get()
  inv<- solve(mat,...)
  x$setInverse(nill)
  inv
}