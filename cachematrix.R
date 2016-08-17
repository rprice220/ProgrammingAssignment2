## makeCacheMatrix creates "matrix" object that can cache inverse
## cacheSolve computes inverse of "matrix" object in makeCacheMatrix, if already
## calculated, retrives inverse from cache

## creates "matrix" object to cache inverse

makeCacheMatrix <- function(x = matrix()) {
  ## intitalize inverse as null
  i <- NULL
  
  ## set agruments to parent environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## define getter for matrix x
  get <- function() x
  
  ## define getter and setter for inverse of matrix x
  setinverse <- function(solve) i <<- solve
  
  getinverse <- function() i
  
  ## assign each element to list in parent environment
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates inverse of matrix x or pulls from cache in 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## if already calculated, return matrix inverse from makeCacheMatrix
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if not already calculated, compute inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
