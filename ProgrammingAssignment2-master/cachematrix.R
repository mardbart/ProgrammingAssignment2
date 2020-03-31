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
## The function adopts a waterfall logic to calculate the inverse 
## only if the matrix is made up of numbers, is square and its determinant is != 0
## if the matrix is noninvertible, inv is not computed

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) { ##checking if the inverse is in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if (class(data[1,1]) == "numeric" | class(data[1,1]) == "integer"){ ##begin of the waterfall logic. Every element is coerced to be of the same class, so [1, 1] is representative!
    if (dim(data)[1] == dim(data)[2]){
      if(det(data) != 0){
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
      }
      else{
        message("the matrix is nonivertible, its determinant is != 0")  
      }
    }
    else{
      message("the matrix is nonivertible, it is not a square matrix!")  
    }
  }
  else{
    message("the matrix is nonivertible, values are not numbers!") 
  }          
}