## These functions cache the inverse of a square invertible matrix.

## Written by AmyB47 @ Github
## Forked from rdpeng/ProgrammingAssignment2

## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        
        get <- function() x
        
        ## The solve function in R computes the inverse of a square matrix
        
        setinverse <- function(solve) v <<- solve
        
        getinverse <- function() v 
        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve function 
## retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        v <- x$getinverse()
        
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinverse(v)
        v
        
}


