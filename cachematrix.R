## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		
		m <- NULL
        
        set <- function(y=matrix()) {

                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinve <- function(solve) m <<- solve
        
        getinve <- function() m
        
        list(set = set, get = get,setinve = setinve,getinve = getinve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
		m <- x$getinve()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinve(m)
        
        m
}
