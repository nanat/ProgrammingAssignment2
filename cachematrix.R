## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  
  get <- function() mat
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(matrix, ...) {
        
  inverse <- matrix$getInverse()
  
  if (is.null(inverse)) {
    data <- matrix$get()
    inverse <- solve(data, ...)
    matrix$setInverse(inverse)
  }
  else print("loading inverted matrix from cache")
  
  inverse
}