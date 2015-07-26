## Programming Assignement 2
## R Programming
## Matrix Inversion and Caching

## The function makes a list containing functions to set a matrix,
## get the matrix, set the value of the inverse, and get the value of
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function checks to see if the inverse is already solved.
## If it is it will return the cached version, if not it will solve
## and then cache the result.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("Value is cached")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
