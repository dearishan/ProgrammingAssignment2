##Cache the inverse of a matrix
## "makeCacheMatrix" and "Cachesolve" function
## makeCacheMatrix- function to create a special "matrix" object
## cache inverse the created "matrix" 

 makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
	setinv = setinv, 
	getinv = getinv)
 }

##cacheSolve -inverse the matrix

 cacheSolve <- function(x, ...) {         
	inv <- x$getinv()
	if(!is.null(inv)) {
	message("getting cached result")
	return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
 }
