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
  ## set the value of matrix to new value
  ## set the inverse for this new matrix to be NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Return the current matrix
  get <- function() x
  ## Set the inverse of matrix x
  setinverse <- function(inverse) inv <<- inverse
  ## Return the inverse of matrix x
  getinverse <- function() inv
  
  ## Return value for makeCacheMatrix
  ## A list of above four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the matrix, i.e., it calculates the inverse
## of the "special matrix" prepared by the above makeCacheMatrix 
## function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the inverse from the cachelist
  inv <- x$getinverse()
  
  ## check if the returned inverse is NULL or not
  if(!is.null(inv)){
    ## message("Getting cached data")
    ## Inverse is not null
    ## return the cached result
    return(inv)
  }
  ## Cached result is NULL
  ## Get the original data matrix x
  data <- x$get()
  ## Compute the inverse of x
  inv <- solve(data)
  ## Cache the result of inverse calculation
  x$setinverse(inv)
  ## Rerturn the result of matrix inverse
  inv
}
