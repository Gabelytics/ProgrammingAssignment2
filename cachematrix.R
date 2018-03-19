#The following program is designed to take a Matrix and compute the Matrix's inverse. If the inverse of a matrix has already been computed,
#the inverse will be taken from cache. If not, a new matrix inverse will be computed.


#This function takes a Matrix and outputs functions which do the following:
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function checks will check if the inverse of the matrix has been calculated, if yes, the function will get the inverse matrix from cache
#if not, the matrix will compute the inverse of the matrix via the setinverse function.

cacheinverse <- function(x, ...) {
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


