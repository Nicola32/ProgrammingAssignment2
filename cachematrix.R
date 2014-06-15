##Caches the inverse of a matrix

## Creates a cachable matrix object
## Returns a list of functions to assign and retrieve cached inverse of x
makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        ## Function 'set' initial matrix
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        ## Function 'get' returns x
        get <- function() x
        ## Function 'setinverse' calculates inverted matrix and stores in cache
        setinverse <- function(solve) invm <<- solve
        ## Function 'getinverse' returns inverted matrix
        getinverse <- function() invm
        ## Returns a list of above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of CacheMatrix or retrieves it from the cache
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## assigns a value to invm, corresponding to inverted matrix
        invm <- x$getinverse()
        ## if invm is not NULL then inverse has already been calculated, get from cache
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        ## else calculate inverse here and set in cache 
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverse(invm)
        invm
        ## In either case, inverted x is returned
}
