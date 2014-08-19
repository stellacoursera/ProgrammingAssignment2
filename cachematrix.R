## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## makeCacheMatrix creates a special "Matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseCache <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse){
    inverseCache <<- inverse
  } 
  getInverse <- function(){
    inverseCache
  } 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve function solve the matrix of the special "matrix" created with the above function 
## and return the inverse matrix.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the solved matrix from the cache and skips the computation. 
## Otherwise, it solve the matrix and  sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
  }else{
    message("solve the matrix")
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }  
  inverse
}
