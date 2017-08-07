

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Creating special functions to get and store/set inverse of the matrix in, following the given example
## I tested the function with $getInverse of Matrix to check if it is null 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checking if the inverse already exists in cache, and if so the inverse will be fetched
## if not, use the solve method to find the inverse and store it in cache using $setInverse (function defined in makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Load Previously Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
