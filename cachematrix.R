## If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, 
## it can be looked up in the cache rather than recomputed.
## Those functions perform exactly that

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix
##get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
  # here we introduce the <<- operator that preserves the value
  # in an environment different than the current one
  x <<- y
  i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i

list(set = set, 
    get = get
    setinverse = setinverse
    getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverese from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverese matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
        ## checking if the inverse has been calculated
  if(!is.null(i)) {
    message ("Getting Cached Data")
    return(i)
  }
  data <- x$get ()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
