## These two functions allow to obtain the inverse of the matrix x, 
## either calculating it or retrieving it from the cache

## This function will create a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix". If the 
## inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv) && inv==solve(x$get())) {
                message("getting cached data and inverting")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
