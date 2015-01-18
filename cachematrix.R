## The below functions cache the result of inverting a matrix
## Basically they will see if the inverse is already calculated,
## and if so then return the value without recalculation.
## Otherwise, the inverse is calculated and then cached for future calls.

## Function creates a special "matrix" which is a list of 4 subfunctions 
##	from an invertible matrix 'x' (note: function assumes input is invertible)
## set() sets the value of matrix x
## get() returns the original value of matrix x
## setinverse() caches the value of the inverse matrix
## getinverse() returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, 
	     get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
}


## Function checks if a "matrix" 'x' created by 'makeCacheMatrix' is already solved for its inversee,
## 	if it has then it returns that inverse, otherwise it calculates the inverse, caches it, 
##	and returns the value.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {	# if inverse has already been calculated
		message("getting cached data")
		return(m)
	}
# remaining steps if m == NULL: they solve for the inverse, cache the result in x, and return it.
	matrix_data <- x$get()		
	m <- solve(matrix_data)
	x$setinverse(m)
	m
}
