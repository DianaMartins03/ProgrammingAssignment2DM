## Put comments here that give an overall description of what your
## functions do

## The following code/functions are used to write 
## an object that stores a matrix and caches its inverse. 
## In the first function (makeCacheMatrix) is created a special matrix, 
## which is a list containing a function to:

## 1st - Set the value of the matrix
## 2nd - Get the value of the matrix
## 3rd - Set the value of the inverse
## 4th - Get the value of the inverse


makeCacheMatrix <- function(m = matrix()) {
  
  t <- NULL
  
  set <- function(y) {
    m <<- y
    t <<- NULL }
  
  get <- function() m  
  
  setinverse <- function(inverse) t <<- inverse
  
  getinverse <- function() t
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function helps to get the inverse of a matrix 
## returned by the function makeCacheMatrix created above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  
  t <- m$getinverse()
  if (!is.null(t)) {
    message("Getting cached data...")
    return(t) }
  
  data <- m$get()
  t <- solve(data, ...)
  m$setinverse(t)
  t
  
}
