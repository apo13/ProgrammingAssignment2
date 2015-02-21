## The following functions calculate and cache the inverse of matrix, to save time by 
## avoiding repeated calculations

## The makeCacheMatrix function creates a special matrix object to store the 
## calculated inverse matrix in later

makeCacheMatrix <- function(x = matrix()) {
    ##create an empty m variable to store calculated inverse of a matrix in later
    m <- NULL 
    ##set the x to y, and m to NULL in the global environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x ## return the value of x
    ## store inverse matrix calculated by solveCache function in m variable
    setinverse <- function(solve) m <<- solve 
    getinverse <- function() m ## return cached inverse matrix
    ## create a list of the functions, assign names to each element of the list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## the following function checks if there is a calculated inverse matrix stored in m 
## variable and if so it returnes the calculated matrix. Othervise it calculates the 
## inverse matrix and stores it in m

cacheSolve <- function(x, ...) {
    ## check if there is an calculated inverse stored in m
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    ## if there's no data stored in m, calculate the inverse of a matrix created by 
    ## makeCacheMatrix function and store it in m 
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}