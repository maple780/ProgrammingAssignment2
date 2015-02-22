## The following pair of functions caches the inverse of a matrix by utilizing
## the scoping rules in R to encapsulate the state of a matrix object.
## I assume for this assignment that the matrix supplied is always invertible.


## makeCacheMatrix() creates a special "matrix" object that can cache its
## inverse; returns a list containing 4 setter and getter functions that save
## the values of the matrix object and the computed inverse

makeCacheMatrix <- function(x = matrix())
{
    ## Initialize the value of the inverse
    inv <- NULL
    
    ## Create setter and getter functions for the special "matrix" object
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    ## Create setter and getter functions for the inverse of the "matrix"
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
        
    ## Create and return a list of all 4 setters and getters
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() computes the inverse of the special "matrix" object created
## by makeCacheMatrix(); if the inverse has already been computed
## (and the matrix has not changed), retrieves it from the cache;
## otherwise, computes it and stores it via the special "matrix" object's
## setInverse() function; returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...)
{
    ## Check the cache to see if the inverse has already been computed
    ## (i.e. it is !NULL); if so, then print a message and return it
    inv <- x$getInverse()
    
    if(!is.null(inv))
    {
        message("Retrieving cached data...")
        return(inv)
    }
    
    ## If the inverse does not exist in the cache, get the "matrix" and
    ## compute the inverse using the solve() function in R
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Set the value of the "matrix" object's inverse in the cache
    ## and return it
    x$setInverse(inv)
    inv
}
