
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  
  ## Method: set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method: get the matrix
  get <- function() {
    x
  }
  
  ## Method: set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method: get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return:  list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return: matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return: inverse if its already set else message
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get: matrix from our object
  data <- x$get()
  
  ## Calculate: inverse using matrix multiplication
  m <- solve(data) #%*% data # Note: invoke matrix multiplication %*%
  
  ## Set:  inverse to the object
  x$setInverse(m)
  
  ## Return: matrix
  m
}

## Test
#a <-rbind(c(1, -1/4), c(-1/4, 1))
#cacheSolve(makeCacheMatrix(a))
