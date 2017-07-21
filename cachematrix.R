#! /usr/bin/Rscript

## R programming assignment 2: This program demonstrates caching of
## computed results and lexical scoping techniques. Sorry for the
## particularly verbose comments. It's a habit.

## Author: John Snapp


## makeCacheMatrix: creates an instance of a matrix with functions to
## cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    # An new instance of makeCacheMatrix accepts a matrix 'x' passed
    # at the command line. We also explicity set m to a null value
    # just to make sure there is no existing value stored there.

    m <- NULL
        
    # This is a class within makeVector called 'set' in the makeVector
    # object. set takes accepts a parameter 'y' and assigns it to
    # value 'x' within the parents scope using the <<- operator.
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # The 'get' function returns the current value of 'x'. Since 'x'
    # is not defined within this instance, this function returns the
    # value of x from the parent environment of makeVector.
    
    get <- function() x

    # The function 'setmean' sets the value for 'm' in the parent
    # environment.
    
    setInverse <- function(solve) m <<- solve

    # This function returns the value of 'm'. Since 'm' is not defined
    # locally, it is returned from the parent environment.
    
    getInverse <- function() m

    # This creates a list of functions that will be retained in the
    # parent environment. Since the elements of the list are all
    # named, they can be easily called with the '$' operator.

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


cacheSolve <- function(x, ...) {

    # 'x' must of an object of type 'makeCacheMatrix', since we need
    # to call the methods of the instance.

    # The getInverse call from the makeCacheMatrix will return the
    # inverse matrix if it has been previously calculate and cached or
    # it will return a null if it has not.
    
    m <- x$getInverse()

    # If we did not get a null, we will return the returned value.
    
    if(!is.null(m)) {
        message("getting cached matrix data")
        return(m)
    }

    # Otherwise, we will get the data from the makeCacheMatrix object
    # calculate the inverse, and save it via the setter function.


    message("calculating inverse matrix")
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m

}


##  Begin execution

# create a square matrix

myMatrix <- matrix(c(1,3,15,19), nrow=2, ncol=2)
matr <- makeCacheMatrix(myMatrix)
cacheSolve(matr)
cacheSolve(matr)
cacheSolve(matr)
