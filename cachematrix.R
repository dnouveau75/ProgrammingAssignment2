## Altogether, these functions allow the user to cache the inverse
## of a sqaure, non-singular matrix.

## The first function takes a matrix as an argument and returns a list of four functions
## which will then be used in the cache function.

makeCacheMatrix <- function(x = matrix()) {
        ##checks whether the matrix has an inverse with two conditions
        if(rcond(x) < 1e-12) {
                message("Your matrix is singular. Please change values")
        }      
        else if (nrow(x) != ncol(x)) {
                message("Your matrix is not square. Please change values")
        }
        else { 
                ##sets the inverse of the matrix to 0 in case get is called
                ##before I is computed with cacheSolve or set
                I <- NULL
                set <- function(y = matrix()) {
                        ##sets x and I in the parent frame
                        x <<- y
                        I <<- NULL
                        ##checks whether the matrix defined with set has an inverse
                        if(rcond(y) < 1e-12) {
                                message("your matrix is singular. Please change values")
                        }
                        else if (nrow(x) != ncol(x)) {
                                message("Your matrix is not square. Please change values")
                        }
                }
                ##get returns the matrix
                get <- function() x
                ##setinverse sets the inverse in the parent frame
                setinverse <- function(inverse) I <<- inverse
                #getinverse returns the inverse of the matrix
                getinverse <- function() I
                ##the function makeCacheMatrix returns a list of four functions
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        }
}

## This function uses the "special" matrix created above to compute and cache
## its inverse

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
        ##returns the inverse of 'x'
        I
}