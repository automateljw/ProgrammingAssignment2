## Here are a pair of functions: makeCacheMatrix() and cacheSolve()

## makeCacheMatrix: creates a special "matrix"

makeCacheMatrix <- function(im = matrix()) {
  inv <- NULL
  set <- function(em) {
    im <<- em
    inv <<- NULL
  }
  get <- function() im
  setinv <- function(einv) inv <<- einv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: calculates the inverse of the special "matrix"

cacheSolve <- function(em, ...) {
        ## Return a matrix that is the inverse of 'em'
  im <- em$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(em)
  }
  data <- em$get()
  im <- solve(data, ...)
  em$setinv(im)
  im
}
