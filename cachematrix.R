
## These two functions will let you calculate Matrix inverse and get result from cache if the operation has already done before.


## Creates a special matrix object that contains all actions in order to cach its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above and returns the result or a cached result if available.
cacheSolve <- function(x, ...) {
  
  ## Get the inverse of matrix if available
  m <- x$getInverse()
  
  ## returns the inverse if already set in cache
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## If not : compute the inverse and stores it into cache
  data <- x$get()
  
  ## Compute inverse of the Matrix
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}