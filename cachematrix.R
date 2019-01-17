# Functions for creating a "special matrix" that can solve and cache its 
# inverse.  
#
# Forked from: https://github.com/rdpeng/ProgrammingAssignment2
#

#
# Creates a "special matrix" i.e. actually a list of functions the can be 
# used for setting/getting a matrix and its inverse
#
makeCacheMatrix <- function(theMatrix = matrix()) {
    
    inversedMatrix <- NULL
    
    set <- function(y) {
        theMatrix <<- y
        inversedMatrix <<- NULL
    }
    
    get <- function() {theMatrix}
    
    setInverse <- function(im) {inversedMatrix <<- im}
    getInverse <- function() { inversedMatrix }
    
    # Return a list with all the methods
    funcs <- list(set = set, get = get, 
                  setInverse = setInverse, getInverse = getInverse)
    invisible(funcs)
}


# 
# Returns the inverse from a "special Matrix", including solving the inverse
# if that hasn't already been done.
# 
cacheSolve <- function(specialMatrix, ...) {
    inverseMatrix <- specialMatrix$getInverse()
    if(!is.null(inverseMatrix) ) {
        message("returning cached value for inverse matrix")
    }
    else {
        inverseMatrix <- solve( specialMatrix$get(), ... )
        specialMatrix$setInverse(inverseMatrix)
    }
    
    # return the inversed matrix
    return(inverseMatrix)
}
