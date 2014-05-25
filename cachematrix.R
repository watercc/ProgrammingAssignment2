#Returns a list of functions used for saving to reading inverse from cache
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	
	#get the input data
        get <- function() x

	#cache the inverse
        setInverse <- function(inverse) m <<- inverse

	#get the inverse from the cache
        getInverse <- function() m

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	#get inverse from cache
	m <- x$getInverse()

        if(!is.null(m)) {
		#if inverse can be retrieved from cache, return the inverse from the cache
                message("getting cached data")
                return(m)
        }
	  
	#if cannot find inverse from cache, calculate the inverse	
        data <- x$get()
        m <- solve(data, ...)

	#save the inverse into the cache
        x$setInverse(m)

	#return the inverse
        m
}
