# Returns list of functions 'set' and 'get' used to access the matrix value
# and functions 'setInv' and 'getInv' used to access cache value
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# If x has calculated inverse matrix in cache returns the cache value
# Otherwise calculates inverse matrix, cache and returns it
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (is.null(inv)) {
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
    }
    inv
}
