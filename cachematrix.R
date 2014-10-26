## Programming Assignment 2: Lexical Scoping

## Below you will find two functions, the makeChacheMatrix and the cacheSolve function.
## These two functions in R enable a cache to be used to reduce the time spent on time-consuming functions.
## If the contents of a vector are not changing, 
## then it may make sense to cache the value as opposed to compute it all over again.
##
## The Cache Matrix
## This function, makeChacheMatrix, creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## The cacheSolve 
## This function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix above.
## if the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}
