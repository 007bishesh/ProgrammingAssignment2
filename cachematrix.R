## The functions below take advantage of scoping rules in R to  
## preserve state inside of R object
## Specifically these functions below are creating a matrix and preserving its inverse into R object


## This function below will perform following operations : 
## 1) set and get the value of original matrix 
## 2) set and get the value of inverse matrix 
## 3) creates a list of all these functions

makeCacheMatrix <- function(x = matrix()) {
		
		inv_matrix <- NULL
        
        set <- function(y=matrix()) {

                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) inv_matrix <<- solve
        
        getinverse <- function() inv_matrix
        
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## The function below will peform following operations :
##
## 1) Checks if inverse of the matrix is already created, if it has already been calculated, 
##    then  it returns the value from cache and stops executing other codes. 
##
## 2) If however the inverse has not been computed , then it calculates inverse by using solve function
##    and preserves the value using setinverse which essentially is using   <<- operator 
##    which can be used to assign a value to an object in an environment that is different from the current environment

cacheSolve <- function(x, ...) {
        
        inv_matrix <- x$getinverse()
        
        if(!is.null(inv_matrix)) {
                message("retrieving cached data")
                return(inv_matrix)
        }
        
        data <- x$get()
        
        inv_matrix <- solve(data, ...)
        
        x$setinve(inv_matrix)
        
        inv_matrix
}
