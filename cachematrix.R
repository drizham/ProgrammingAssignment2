# cachematrix.R implements accepts a matrix input and creates a 
# special matrix object that is able to cache its own inverse
# example usage:
# 	source("cachematrix.R")
#	m0 <- matrix(c(1, 4, 9, 16), 2,2)	# create matrix
#	m1 <- makeCacheMatrix(m0)			# create 'special' matrix
#	cacheSolve(m2)			# calculate inverse of matrix
#	cacheSolve(m2)			# calling cacheSolve again retrieves previously
#							# calculated inverse


# makeCacheMatrix accepts a matrix and creates an object that
# can cache its' inverse. i.e. save its own inverse itself

makeCacheMatrix <- function(x = numeric()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

# cacheSolve returns the inverse of a cacheMatrix object
# if called for the first time it calculates and stores the inverse
# if called after that it returns the previously calculated & stored inverse

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
