## These 2 functions are used to create a special object that stores a matrix and cache's its inversion. 

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
	set <- function(y){
	    x <<- y
            inv_m <<- NULL
	}
	get <- function() x
	set_inversion <- function(solve) inv_m <<- solve
	get_inversion <- function() inv_m
	list(set = set, get = get, set_inversion = set_inversion,
	     get_inversion = get_inversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      	## Return a matrix that is the inverse of 'x'
	inv_m <- x$get_inversion()
	if(!is.null(inv_m)){
		message("getting cached data")
		return(inv_m)
	}
	data <- x$get()
	inv_m <- solve(data)
	x$set_inversion(inv_m)
	inv_m	
}
