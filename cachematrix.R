## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. This code is written for second assignment of Course 2.
## it gets a square matrix and it uses solve function to get inverse matrix and it caches result.

## The function below gets a square matrix as argument and it produces a list of functions 
## aiming to set and get matrices (normal and inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y){
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv_mat <- function(solve) inv_mat <<- solve
  getinv_mat <- function() inv_mat
  list(set = set, get = get, setinv_mat = setinv_mat, getinv_mat = getinv_mat)
}


## This function receives a matrix and it checks if the inverse matrix was already calculated or not.
## If inverse matrix is cached, it returns cached matrix;if it´s not, then it computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv_mat()
  if (!is.null(inv_mat)){
    message("getting cached data")
    return (inv_mat)
  }
  data <- x$get()
  inv_mat <- solve (data, ...)
  x$setinv_mat(inv_mat)
  inv_mat
}
