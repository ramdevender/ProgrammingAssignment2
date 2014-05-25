#This function, makeCacheMatrix creates a special "matrix"




## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set the values of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get the values of matrix
  get <- function() x
  
  #set the value of inverse matrix
  setsolve <- function(solve) m <<- solve
  
  #get the value of inverse matrix  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


#The following function calculates the mean of the special matrix
#created with the above function. However, it first checks to see 
#if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value
#of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  #get the inverse of the matrix
  
  m <- x$getsolve()
  #If exists already
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If NOT exists, gets the data
  data <- x$get()
  
  #get inverse of matrix
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
