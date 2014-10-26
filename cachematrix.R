## Altogether, these functions allow the user to cache the inverse
## of a sqaure, non-singular matrix.

## The first function takes a matrix as an argument and returns four functions
## which will then be used in the cache function.

makeCacheMatrix <- function(x = matrix()) {
        ##checks whether the matrix has an inverse
        if(rcond(x) < 1e-12) {
                message("Your matrix is singular. Please change values")
        }      
        else if (nrow(x) != ncol(x)) {
                message("Your matrix is not square. Please change values")
        }
        else { 
                I <- NULL
                set <- function(y = matrix()) {
                        ##sets x and I in the parent frame
                        x <<- y
                        I <<- NULL
                        ##checks whether the matrix has an inverse
                        if(rcond(y) < 1e-12) {
                                message("your matrix is singular. Please change values")
                        }
                        else if (nrow(x) != ncol(x)) {
                                message("Your matrix is not square. Please change values")
                        }
                }
                ##list of four functions to be used in cacheSolve
                get <- function() x
                setinverse <- function(inverse) I <<- inverse
                getinverse <- function() I
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        }
}

## This function uses the "special" matrix created above to compute and cache
## the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        ##checks whether the cache already contains the inverse
        if (!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        ##computes and caches the inverse of the matrix
        data <- x$get()
        I <- solve(data,...)
        x$setinverse(I)
        I
}
