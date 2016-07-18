## makeCacheMatrix creates a special matrix
## thas is a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the inverted matrix
## - get the inverted matrix
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


## cacheSolve returns a matrix that is the inverse of the input 'matrix'
## if the inverted matrix is not yet solved, it will be calculated and returned
## otherwise, it will be just returned
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
