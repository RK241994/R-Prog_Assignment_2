## This assignment is about solving the inverse of a matrix by caching
# previously calculted inverse using two set of functions: makeCacheMatrix
# and cacheSolve


## makeCacheMatrix : Creates a new environment.
# Output of this function is a list: set, get, setinverse
# and getinverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix_exg <- x$get()
  m <- solve(matrix_exg,...)
  x$setinverse(m)
  m
}
