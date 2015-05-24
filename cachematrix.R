## The two functions below serve to "process" a matrix so that 
## an inverse is outputted and stored for future use


## The makeCacheMatrix function is essentially a holding pen;
## It's contents are later referenced by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, 
	    setinverse = setinverse, 
	    getinverse = getinverse)
}


## The cacheSolve function searches the makeCacheMatrix function
## to see if the desired inverse is stored. If not, it computes it.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("retrieving cached inverse")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix)
	x$setinverse(inv)
	inv
}

## In practice, it would probably be best to stitch the two functions
## together so that all a person had to do was input a matrix and the
## program would figure out whether the corresponding inverse had been
## stored or if it needed to be computed. Since we were given a stub
## to start out with, I tried to adhere to the format the instructions
## seemed to put forth.