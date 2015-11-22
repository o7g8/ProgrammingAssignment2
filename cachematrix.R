## A pair of fucntions makeCacheMatrix() and cacheSolve() which
## compute inverse matrix in an efficient wway using caching of results.

## Create an cache matix in a form of a list containing
## 'get' and 'set' functions getting and returning value of the matrix and
## 'setinverse' and 'getinverse' functions getting and setting value of
## the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## A function returning iverse of the given cached matrix (created with
## makeCacheMatrix() function). 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
