## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix The function takes a square invertible matrix as its input. 
##                 The function returns the following list of method functions:
##                 set -- This function will store it's input y in cache location 
##                        for x. Also it stores a NuLL value in the cache location
##                        allocated for the inverse matrix
##                 get -- Returns the input matrix 
##                 setmatrix -- Stores a matrix in cache
##                 getmatrix -- Returns the matrix m
##
## cacheSolve      This function takes the list of method functions created by 
##                 makeCacheMatrix as it's input. It retrieves the matrix m
##                 and checks to see if it is a NULL. If it is Not a NULL, 
##                 then it returns the matrix value m that was previously 
##                 stored in cache. If it is a NULL, then calculates the 
##                 inverse and then stores the value in cache and returns m.
##                 
##                 

## Write a short comment describing this function
## Returns a list of method functions for storing the input
## matrix x and it's inverse matrix m in local memory or
## cache memory.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(mat) m <<- mat
  getmatrix <- function() m
  list(set = set, get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## Write a short comment describing this function
## Uses the method functions from makeCacheMatrix
## to check for the inverse matrix of x. If it
## exists, then it retrieves the value from cache.
## If it doesn't, then it calculates the inverse
## and stores it cache and returns m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of'x'
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}

