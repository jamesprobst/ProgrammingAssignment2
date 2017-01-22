## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and a second function that returns the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##create a function which starts with a null matrix argument
        inverse <- NULL
        ## delcare another function set where the value will be cached in 1. Matrix is created
        ## for the first time. 2. changes made to cached matrix
        set <- function(y) {                      
                x <<- y
                ## change the value of inverse of the matrix in case the matrix was changed.
                inverse <<- NULL              
        }
        ## gets the value of the inverse
        get <- function() x                           
        ## set the value of the matrixinverse to inverse
        setinverse <- function(matrixinverse) inverse <<- matrixinverse 
        ## gets the inverse     
        getinverse <- function() inverse        
        ## passes the value of the function makeCacheMatrix        
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("get cached data")
                return(inverse)
        }
        ## if not inverse matrix, calculate and retrieve
        new <- x$get()
        inverse <- solve(new)
        x$setinverse(inverse)
        inverse
}