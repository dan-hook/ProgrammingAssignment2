## Matrix inversion is an expensive operation.  If a program has to calculate the inverse of 
## the same matrix multiple times, it makes sense to cache the results.  The first function below
## creates an object that contains a matrix and can save a value.  The second function takes an object
## constructed by the first, and returns the inverse.

## This is a constructor for an object that has the following functions:
# 1) set - set the value of the matrix; argument must be a matrix
# 2) get - returns the value of the matrix
# 3) setinverse - sets a value that is returned by the next function
# 4) getinverse - returns the value set by the last function, or NULL if not set
#Note that while the functions have "inverse" in the name, this constructor and the object it creates
#do nothing to enforce any relationship between the matrix (the value returned by get) and the "inverse"
# (the value returned by getinverse)

makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(y) {
      x <<- y
      matrix_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) matrix_inverse <<- inv
    getinverse <- function() matrix_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##This takes an object produced by the above function and returns the inverse of the matrix.  If 
##the provided object has a cached version of the inverse, it uses that version.  If not, it 
##computes the inverse and stores it back in the object.
cacheSolve <- function(x, ...) {
    matrix_inverse <- x$getinverse()
    if(!is.null(matrix_inverse)) {
      message("getting cached data")
      return(matrix_inverse)
    }
    data <- x$get()
    matrix_inverse <- solve(data, ...)
    x$setinverse(matrix_inverse)
    matrix_inverse
}
