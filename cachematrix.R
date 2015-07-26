## Functions used to cache the inverse of a matrix instead of computing
## everytime it is required

## Creates the auxiliary functions list from example adapted to a matrix scenario

makeCacheMatrix <- function(x = matrix()) {
		inversa <-NULL
		set <- function(y) {
			x <<- y
			inversa <<- NULL
		}
		get <-function() x
		setsolve <- function(solve) inversa <<- solve
		getsolve <- function() inversa
		list( set=set, get= get, setsolve=setsolve, getsolve=getsolve)
}


## Calculates the inverse of a matrix or, if already calculated,  
## gets it from cache

cacheSolve <- function(x, ...) {
		inversa <- x$getsolve()
		if(!is.null(inversa)) {
			message("Getting cached inverse")
			return(inversa)
		}
		matriz <- x$get()
		inversa <- solve(matriz, ...)
		x$setsolve(inversa)
		inversa
}
