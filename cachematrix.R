# Functions that cache the inverse of a matrix

# 1.0 Creates a matrix that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  # 1.1 Initializing the inverse property
  i <- NULL
  
  # 1.2 Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # 1.3 Getting the matrix
  get <- function() {
    m
  }
  
  # 1.4 Setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # 1.5 Getting the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  # 1.6 Returning a list of the methods
  return(list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}


# 2.0 Computes the inverse of the special matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  
  # 2.1 Returning a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # 2.2 Returning the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # 2.3 Getting the matrix from our object
  data <- x$get()
  
  # 2.4 Calculating the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # 2.5 Setting the inverse to the object
  x$setInverse(m)
  
  return(m)
}

