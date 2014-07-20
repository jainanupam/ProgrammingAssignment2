## Following functions are used to cache the inverse of
## a given matrix.

## makeCacheMatrix function creates a "special matrix" 
## which is actually a list of four functions:
## 1. set(y) to set the value of the matrix
## 2. get() to get the value of the matrix
## 3. setinverse(inverse) to set the inverse of the matrix
## 4. getinverse() to get the inverse of the matrix (cached)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the matrix, i.e., it calculates the inverse
## of the "special matrix" prepared by the above makeCacheMatrix 
## function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
