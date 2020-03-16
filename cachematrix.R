
## Create a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
             
	        ## Set value of special "matrix"
                set <- function(y) {
                    x <<- y
                    m <<- NULL
        	}
			
	        ## get value of special "matrix"
		get <- function() x

		## set inverse of special "matrix"
		setinv <- function(inv_m) m <<- inv_m

		## get inverse of special "matrix"
		getinv <- function() m

		list(set = set, get = get,
		     setinv = setinv,
                     getinv = getinv)
}


## Function to retrieve or compute inverse of
## special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinv()
        if(!is.null(m)) {
	       message("getting cached matrix inverse")
               return(m)
        }
        spec_mat <- x$get()
        m <- solve(spec_mat, ...)
        x$setinv(m)
        m

}
