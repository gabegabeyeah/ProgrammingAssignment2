# Gabe Schwartz
# Coursera - R Programming
# Assignment 2 (Lexical Scoping)


#---------------------
# makeCacheMatrix:
#   - caches a given matrix (null by default)
#   - creates a null matrix 'm' to store the inverse of that matrix
#   - creates a list of functions (mutators and accessors) that allow cacheSolve to avoid 
#      re-calculating the inverse of the given matrix if we want to calculate it again


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

            
#---------------------
# cacheSolve:
#   - checks to see if we've already calculated the inverse of a matrix (cached as 'm')
#   - returns the cached inverse if we have
#   - calculates it otherwise (and caches it)

            
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data...")
            return(m)
        }
        message("no cached data found - solving...")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
