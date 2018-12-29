## As the homework requires, these functions allow for a user to store a matrix and 
## calculate its inverse only one time, with cacheSolve holding the results of the 
## first matrix inversion

## This function returns a list of the 4 funtions used to set a matrix, get a matrix, 
## set its inverse, and get its inverse. The logic is practically a copy of the 
## cacheVector() example provided for this homework, with modifications 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## As with the previous function, this one is very similar to the cachemean() function,
## with the main change being the use of solve() to calculate the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Assume that 'data' is a square, invertible matrix
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
