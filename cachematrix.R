## Put comments here that give an overall description of what your
## functions do
## 

## The makeCacheMatrix function takes a matrix as input. First, it clears the cached inverse data to
## avoid giving wrong historical data. The function then takes the input matrix and creates a matrix,
## its inverse, and caches that data.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function()x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The cacheSolve function looks at the cached inverse matrix in the list object created in 
## makeCacheMatrix and returns the value that is cached if it is not null. If it is null, it
## takes the time to calculate the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}
