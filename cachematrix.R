## We write two functions to create a special "matrix" that stores both the 
## matrix and its inverse.

## makeCacheMatrix creates an environment that stores a matrix 'x' as well as
## it's inverse.  Functionally, it returns a list of the functions required to 
## get and set the values of each.

makeCacheMatrix <- function(x = matrix()) {
    # By default the inverse of x is not known, so set to NULL
    xinv <- NULL
    
    # 'set' changes the stored matrix 'x' and resets the inverse to NULL
    set <- function(y){
        x <<- y
        xinv <<- NULL
    }
    
    # 'get' returns the matrix contents
    get <- function() x
    
    # 'setInverse' caches the provided inverse of 'x'
    # Note, do not call directly!  Use cacheSolve to set the inverse
    setInverse <- function(inverse) xinv <<- inverse
    
    # 'getInverse' returns the cached inverse of 'x' (NULL if inverse has 
    #   not yet been calculated)
    getInverse <- function() xinv
    
    # makeCacheMatrix returns a list of functions for manipulating the 
    #   matrix and inverse stored within the environment
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve returns the inverse of a matrix 'x' created with makeCacheMatrix.
## If the inverse has been previously cached, it is returned, otherwise the
## function computes the inverse and caches it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # First check whether inverse is already cached
        xinv <- x$getInverse()    # get current inverse
        if (!is.null(xinv)){    # if cached inverse exists, return it
            message("getting cached data")
            return(xinv)
        }
        
        # If not cached, solve for inverse of 'x' and cache result
        data <- x$get()    # get contents of 'x'
        xinv <- solve(data, ...)    # calculate the inverse
        x$setInverse(xinv)    # cache calculated inverse
        
        # Finally, return the inverted matrix
        xinv
}
