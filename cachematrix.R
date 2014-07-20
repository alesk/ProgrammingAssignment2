## File description
##
## There are two functions defined in this file:
##
## - makeCacheMatrix(matrix) that wraps a matrix
##   and provides caching for the inversion of matrix
##
## - cacheSolve(cacheMatirx) returns inverse of cacheMatrix.
##   If inverse was already calculated and the matrix has not been
##   changed, the cached result is returned, avoiding intensive
##   computation.
##
## Testing:
##  
##     m1 <- matrix(c(1,4,2,1), nrow=2, ncol=2)
##     m2 <- matrix(c(1,4,2,1), nrow=2, ncol=2)
##     mc <-makeCacheMatrix(m1)
##
##     cacheSolve(mc) # calculates inverse
##     cacheSolve(mc) # returns cached inverse
##      
##     mc$set(m2) # matrix is reset
##     cacheSolve(mc) # calculates inverse
##     cacheSolve(mc) # returns cached inverse



## Creates a wrapper class around matrix that
## caches inversion of matrix.
## 
## Use:
##  - get() to retrieve the wrapped matrix
##  - set(matrix) to change the wrapped matrix
##  - getinverse() to retrieve cached inverse of matrix
##  - setinverse() to set cached inverse of matrix
##
##  The getinverse() and setinverse() are meant only to be used by
##  accompaining cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        # Binds the value of y to x in parent scope
        x <<- y
        
        # Resets the cached inverse (from parent scope) to NULL
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(y) inverse <<- y
    getinverse <- function() inverse        
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of matrix x.

cacheSolve <- function(x, ...) {
    
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
            message("Getting inverse from the cache.")
            
            # Returns cached version of the  inverse of matrix x
            return(inverse)
        }
        
        # Compute, write to cache and return inverse of matrix x
        inverse <- solve(x$get())
        x$setinverse(inverse)
        inverse
}
