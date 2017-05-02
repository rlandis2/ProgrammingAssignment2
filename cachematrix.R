## The goal of this file is to demonstrate functions that utilize lexical scoping within R. 
## The first function initializes our variables and defines the behaviors for the second function. 
## The second function will test to see if an argument is cached in the environment. 
## It will return the cached answer if available, or calculate a new one if not. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # this first 'set' function intializes our variables
  m <- NULL
  
  # the next section of code defines the behaviors of the objects in the function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  # this section of code names the objects of our list so that the $ operator 
  # can be used in cacheSolve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}




## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # this attempts to retrieve the inverse of the argument 
  m <- x$getsolve()
  
  # this checks if the result is NULL, which means it will have to recalculate the inverse if it is. 
  # If it is not NULL, then it means the inverse is cached and it will return the inverse quickly. 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if the argument above is NULL, this section will calculate the inverse. 
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

