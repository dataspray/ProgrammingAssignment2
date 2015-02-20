## The makeCacheMatrix and cacheSolve functions below implement a special
## object that stores a matrix and caches its mean.

## Return a list of four functions that will 1) set the value of the matrix 
## 2) get the value of the matrix 3) set the value of the inverse, and 4) get 
## the value of the inverse
makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL  # initialize our inverse matrix
  
  ## function to set the value of the matrix  
  set <- function(y) 
  {
    x <<- y     # assign to x in parent enviornment
    i <<- NULL  # assign to i in parent enviornment
  }
  
  ## function to get the value of the matrix
  get <- function() x
  
  ## function to set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## function to get the value of the inverse
  getinverse <- function() i
  
  ## return list of functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Make use of the makeCacheMatrix function above to retrieve the inverse
## of matrix if previously cached, If not, cacheSolve computes the inverse 
## and caches it using makeCacheMatrix's setinverse function
cacheSolve <- function(x) # x = list created with makeCacheMatrix
{
  ## if previously cached, retrieve it, print message, and return it
  i <- x$getinverse() 
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i) # nothing more to do         
  }
  
  ## else, compute the inverse, cache it, and return it
  data <- x$get()   # the matrix 
  i <- solve(data)  # compute inverse with solve
  x$setinverse(i)   # cache the value
  i                 # return it
}
