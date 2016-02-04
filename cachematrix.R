## makeCacheMatrix creates a vector of functions to (a) set a matrix, (b) get a matrix, (c) set inverse of matrix, and (d) get ## inverse of matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y=matrix()) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve inverts a matrix if the invert does not alread exists in the cache.  If it exists in the cache, it simply ## retrieves the invert without recacluating it.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
