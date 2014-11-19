## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# private storage of inverse, initially set to NULL
	# ... as inverse has not yet been calculcated
	inverse <- NULL
	
	# function allowing value of matrix to be reassigned.
	# we reset inverse to NULL as inverse will need to be
	# recalculated for new matrix value
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# getter method - retrieve current matrix value
	get <- function() x

	# we'll use this to cache inverse after calculation
	setinverse <- function(inv) inverse <<- inv

	# getter for caculated inverse
	getinverse <- function() inverse
	
	# return list of 4 functions defined above
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
