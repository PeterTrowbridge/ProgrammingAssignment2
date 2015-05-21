## The functions makeCacheMatrix and cacheSolve are 
## two functions designed to work together to store 
## and invert large matrices. The results from 
## makeCacheMatrix are fed to cacheSolve to return 
## the inverted matrix. If cacheSolve has been ran 
## previously, the cached inverted matrix will be 
## accessed instead of recalculating. This is done 
## to reduce load times.

## These functions are heavily based on the example 
## code for this assignment.

## This function, makeCacheMatrix, takes a matrix, x, 
## and returns a vector of functions that are used to 
## return x, return its inverse, set x, and set the 
## inverse. Before cacheSolve is used, the get inverse 
## function returns a NULL value.

## argument x is a matrix
makeCacheMatrix <- function(x = matrix()) {

	## the inverse i is set to NULL
	i <- NULL

	## the function 'set' will replace the stored
	## matrix with a new matrix. It also removes
	## the stored inverse since it become invalid.
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	## returns the matrix x
	get <- function() x
	
	## sets the argument as the inverse, i
	setinverse <- function(x) i <<- x

	## returns the inverse, i
	getinverse <- function() i

	## returns the list of functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function, cacheSolve, takes the results of
## makeCacheMatrix and tries return the inverse of
## the containing matrix. If the inverse already
## exists in the list, then it is returned. Otherwise
## the inverse is calcuated, returned, and cached into
## the list.

## '...' arguments to be passed to solve() function
cacheSolve <- function(x, ...) {

	## set i as current stored inverse of x. This
	## may be NULL or a calculated inverse.
	i <- x$getinverse()

	## check if i is NULL
	if(is.null(i)){

		## if i is NULL

		## notify user of calculations
		message("calculating...")
		
		## solve inverse of x and set as i
		i <- solve(x$get(), ...)
		
		## set inverse in x as i
		x$setinverse(i)
	
	} else {

		## if i is not NULL (already contains inverse)

		## notify user of cache access
		print('getting inverse from cache...')
	}

	## return inverse from x
	return(x$getinverse())
}