## This script 'cachematrix' contains two functions that
## store the inverse of a matrix and caches the data and the inverse.
## The two functions stored by this script are 'makeCacheMatrix' and 'catchSolve'


## START makeCacheMatrix
## This function stores 4 functions: 'set', 'get', 'setinverse', and 'getinverse'.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {                           
                ##'set' determines the matrix used in the function
                x <<- y
                m <<- NULL
        }
        get <- function() x                            
        ##'get' returns the value of the matrix
        
        setinverse <- function(inverse) m <<- inverse  
        ##'setinverse' will store/cache the inverse of the matrix
        
        getinverse <- function() m                     
        ##'getinverse' returns value of matrix inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##End of makeCacheMatrix function


## Start of cacheSolve function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix, If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        ## looks up value of 'getinverse' in makeCacheMatrix function
        ## if value is null, cacheSolve will calculate the inverse and 
        ##store it in 'setinverse' from makeCacheMatrix.
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## inverse of matrix is stored in 'setinverse' from makeCacheMatrix
        m
}
##End of cacheSolve function