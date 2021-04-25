## Making a matrix that can cache its inverse
makeCacheMatrix <- function( m2 = matrix() ) {
  
  ## Initializing the inverse property
  i1 <- NULL
  
  ## Setting the matrix
  set <- function( matrix ) {
    m2 <<- matrix
    i1 <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    m2
  }
  
  ## Setting inverse of the matrix
  settingInverse <- function(inverse) {
    i1 <<- inverse
  }
  
  ## Getting nverse of the matrix
  gettingInverse <- function() {
    ## Return the inverse property
    i1
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       settingInverse = settingInverse,
       gettingInverse = gettingInverse)
}

## If the inverse is already calculated (and the matrix did not
## change), then the "cachesolve" should retrieve inverse from the cache.
cacheSolve <- function(x, ...) {
  
    ## Returning matrix which is inverse of 'x'
    m2 <- x$gettingInverse()

    ## Just return the inverse if its already set
    if( !is.null(m2) ) {
            message("getting cached data")
            return(m2)
    }

    ## Geting matrix from our object
    data <- x$get()

    ## Calculate inverse using matrix multiplication
    m2 <- solve(data) %*% data

    ## Set inverse to the object
    x$settingInverse(m2)

    ## Return the matrix
    m2
}
x <- matrix(c(1,2,3,4),2,2)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
