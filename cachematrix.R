## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix defines the following functions for manipulating a special matrix:
##   - set: sets the value of the matrix
##   - get: returns the cached value
##   - setinverse: uses the solve() to calculate the inverse of the matrix
##   - getinverse: returns the previously calculated inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse to null
    inv <- NULL
    
    set <- function(y) {
        x <<- y ## assign the y matrix to the data matrix of x
        inv <<- NULL ## its inverse is null until it is calculated
    }
    
    get <- function() { ## simply return the data matrix x
        x
    }
    
    
    setinverse <- function(theInv){ ## a matrix has been passed to be stored in the cached
        inv <<- theInv
    }
    
    getinverse <- function() { ## simply return cached inverse matrix
        inv
    }
    
    
    ## return a vector list of these functions
    list (
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Write a short comment describing this function
##
##  the functions invoves getinverse to retrieve the cached inverse
##  if it is null, then it has not been calculated
##      in that case, calculate and store in the cache
##  if it was found in the cache, then take the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv=x$getinverse()
    if (!is.null(xinv)) {
        message("Returning previously cached inverse")
        return (xinv)
    }
    
    ## null means that it has not yet been calculated
    ## calculate and store in cache
    ## first retrieve the x
    xdata <- x$get()
    ## then calculate the inverse
    xinv <- solve(xdata, ...)
    ## store the newly calculated inverse in the cache for next time
    x$setinverse(xinv)
    ## now return the calculated inverse
    xinv
}
