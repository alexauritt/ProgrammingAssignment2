## calculates a special 'matrix', represented by a list containing
## the following four functions to:
##   1) set the value of the matrix
##   2) get the value of the matrix
##   3) set the inverse of the matrix (i.e. cache the inverse)
##   4) get the (cached) inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	
	# private storage of inverse, initially set to NULL
	# ... as inverse has not yet been calculcated
	inverse <- NULL
	
	# function allowing value of matrix to be reassigned.
	# we reset inverse to NULL as inverse will need to be
	# recalculated for new matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# getter method - retrieve current matrix
	get <- function() x

	# setter to cache inverse after calculation
	setinverse <- function(inv) inverse <<- inv

	# getter for pre-caculated inverse (may be null)
	getinverse <- function() inverse
	
	# return list of 4 functions defined above
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	# retrieve cached inverse of x (may be null)  
	m <- x$getinverse()

	# if cached value is not null, use it, and print msg
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	# otherwise,retrieve matrix value, compute and store inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)

	# return newly computed inverse
	m
}
