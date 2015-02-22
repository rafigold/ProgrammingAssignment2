## This function takes a square matrix and returns the inverse. The matrix is also cached to the environment
## which allows it and the inverse (if it has been previously calculated) to be referenced without needing to be recalculated.


## The variable x is used to hold the matrix to be inverted and m is used to hold the inverse.
## m gets reset to NULL if input matrix changes



makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	set <- function(y){
		x <<- y # saves the matrix so that cacheSolve can check whether or not it has been updated
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m


	# create a list of 4 functions defined above 
 
	list(set = set, get = get,
	setmatrix = setmatrix,
 	getmatrix = getmatrix)
}

	
cacheSolve <- function(x, ...) {
	## Returns a matrix that is the inverse of 'x'

	m <- x$getmatrix()
	
	# check if inverse has been calculated and matrix remains unchanged
	if(!is.null(m)){
		message("getting cached data")
		return(m)
		}
	
	matrix <- x$get()

	m <- solve(matrix, ...) # solve calculates the inverse of a square invertible matrix 

	x$setmatrix(m) # set x = m for checking next time the function is run

	m # return m as the inverse matrix solution
}

