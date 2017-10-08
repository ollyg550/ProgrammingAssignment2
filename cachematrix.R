## makeCacheMatrix is basically a function which 
## caches a matrix and its' inverse.
## It defines how you can interact with it
## through get(),set(matrix),setinverse(inverse) and getinverse()
## functions

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function()
		x
	setinverse <- function(inverse)
		i <<- inverse
	getinverse <- function()
		i
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## This function utlizes the makeCacheMatrix to check
## for a cached inverse and if the inverse is not cached
## it will calculate it and update the cache with the inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
