
#############  Programming Assignment 2 ##############################


# The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function 
# to set and get the value of the matrix, to set and inverse value of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



# The second function, cacheSolve gets the inverse of the special "matrix" created with the above first 
# function. It first checks to see if the inverse has already been done. If so, it gets the inverse from the cache
# and skips the process. Otherwise, it calculates the inverse of the matrix and sets the inverse value in the cache
# via the setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


# Validation of the 1st and 2nd functions

m <- matrix(rnorm(16),4,4)  # Create a 4 x 4 matrix
round(m,3)

m1 <- makeCacheMatrix(m)    # Check cached m)atrix
m1

s <- cacheSolve(m1)         # Check inversed matrix
round(s,3)

round(m%*%s,2)              # Check whether the mutiplication of the original matrix and inversed matrix is
                            # a unit matrix.
