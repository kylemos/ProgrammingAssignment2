## Functions that compute the inverse of a (non-singular) square
## matrix, checking first to see if the inverse has already been
## calculated and cached.

## Function: creates a matrix object that caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Function: computes the inverse of the matrix returned by
##		makeCacheMatrix, but checks first to see if the
## 		inverse has already been cached (and returns it).

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
