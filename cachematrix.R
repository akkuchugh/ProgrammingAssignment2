## A pair of functions that cache the inverse
## of a matrix

## It creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {

  ##Initialize
  i<-NULL
  
  ## Method to set the matrix
  set<-function(matrix) {
      m<<-matrix
      i<<-NULL
  }
  
  ##Method to get the matrix
  get <- function() {
    ##Return the matrix
    m
  }
  
  ##Methodto set the inverse of a matrix
  setInverse<-function(inverse) {
      i<<-inverse
  }
  
  ##Method to get the inverse of the matrix
  getInverse<-function() {
      i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
  
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
