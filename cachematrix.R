## The two functions compute and cache the inverse of a matrix 
## to cut back on long computations.

## makeCacheMatrix function creates the special "matrix"

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function()x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set=set,get=get,
	setinverse=setinverse,
	getinverse=getinverse)
}


## cachesolve function calculates the inverse of the matrix created
## in makeCacheMatrix but first determines if the inverse has 
## already been calculated and the pulls the value from the cache

cacheSolve <- function(x = matrix(),...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data,...)
	x$setinverse(inverse)
	inverse
}
	

