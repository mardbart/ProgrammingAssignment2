## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ##setting the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##getting the matrix
  setinv <- function(inverse) inv <<- inverse ##setting the inverse of the matrix
  getinv <- function() inv ## getting the inverse of the matrix
  list(set = set, get = get, ##returning a list of the functions inside makeCacheMatrix's environment
       setinv = setinv,
       getinv = getinv)
}



## this function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) { ##checking if the inverse is in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}