#Programming Assignment 2: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly.

# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse, 
# which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		# We need to clear computed inverse when matrix is changed
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}

# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by
# "makeCacheMatrix" above. If the inverse has already been calculated,
# then the cachesolve should retrieve the inverse from the cache.

# For this assignment, assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
	# Get the value of inverse of the matrix (call makeCacheMatrix$getsolve)
	s <- x$getsolve()
	
	# Checking if there is a cached value
	if(!is.null(s)) {
		message("getting cached data")
		
		# Return the cached value
		return(s)
	}
	
	# If there is no cached value, we need to calculate it.
	
	# Getting the value of the matrix (call makeCacheMatrix$get)
	data <- x$get()
	
	# Function "solve()" returning the inversion of matrix.
	# See "?solve" for more info.
	s <- solve(data, ...)
	
	# Setting the value of inverse of the matrix (call makeCacheMatrix$setsolve)
	x$setsolve(s)
	
	# Return the calculated value
	s
}