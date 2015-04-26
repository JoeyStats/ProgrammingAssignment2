
## The following function cache's the inverse of a matrix, so 
## that it can be used in computations without re-computing it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
	       x <<- y
	       m <<- NULL
	}
	get <- function() x
	setInv <- function(solve) m <<- solve
	getInv <- function() m
	list(set = set, get = get,
	     setInv = setInv,
	     getInv = getInv)
}


## The following function retrieve's the inverse matrix 
## that was cache'd in the previous function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
	m <- x$getInv()
	if(!is.null(m)) {
	        message("getting cached data")
		return(m)
        }
	data <- x$get()
	m <- solve(data, ...)
	x$setInv(m)
	m
}
