## Here are a pair of functions: makeCacheMatrix() and cacheSolve()

## makeCacheMatrix: creates a special "matrix"
## Usage:
##      Y <- makeCacheMatrix(X) # X is a square invertible matrix
##      Y$get()   # get Y matrix
##      Y$set(Z)   # assign matrix Z to Y   
##      Y$getinverse()  # get Y's inverse matrix
##      Y$setinverse(Z)  # assign matrix Z to Y's inverse matrix

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(tm) {
    mat <<- tm
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(tm) inverse <<- tm
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: calculates the inverse of the special "matrix" created by makeCacheMatrix
## Usage:
##      

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'em'
  inverse <- mat$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data, ...)
  mat$setinverse(inverse)
  inverse
}
