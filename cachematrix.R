## makeCacheMatrix generates a ("special matrix"), a list of functions (set, get, setInverse, and getInverse) for an inputted matrix. 
## cacheSolve verifies if an inverse has been calculated before. If not, it calculates, then caches the result.

## makeCacheMatrix generates a "special matrix" from an inputted one.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## cacheSolve caches the inverse matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv

}