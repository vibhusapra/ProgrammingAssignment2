## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object
## that can cache its inverse.
makeCasheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function( matrix ) {
                m <<- matrix
                inv <<- NULL
        }
        get <- function() m
        setInverse <- function (inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse
        )
}


## Write a short comment describing this function


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

casheSolve <- function (m,...) {
        inv <- m$inverse()
        if (!is.null(inv)) {
                message("getting cashed data")
                return (inv)
                
        }
        mat <- m$get()
        inv <- solve(mat, ...)
        m$setInverse(inv) 
        inv
}

