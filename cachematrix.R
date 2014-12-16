## The functions below, take and invertible matrix as input, the matrix and its inverse will be cached.  This allows for fast
## retrieval of the matrix and its inverse.

## Example
# mdat <- matrix( c(1,0,0, 1, 1, 1, 0,0,1), nrow = 3, ncol = 3)
#
# cacheSolve(makeCacheMatrix(mdat))
#      [,1] [,2] [,3]
# [1,]    1   -1    0
# [2,]    0    1    0
# [3,]    0   -1    1


## This function creates a special "matrix" object that can cache its inverse. For this assignment the input should be a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  If the inverse has
##  has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
