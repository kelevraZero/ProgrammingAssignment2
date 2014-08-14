## Put comments here that give an overall description of what your
## functions do
## _This script permit to create an object makeCacheMatrix which can, for a 
## given matrix, save the inverse in cache and return the value without 
## computation if asked.  

## Write a short comment describing this function
## _MakeCacheMatrix : Create an object which implement method to set/get 
## the matrix and his inverse 

makeCacheMatrix <- function(x = matrix()) {
    # Inverse value variable
    inv <- NULL
    
    # _set is called each time we ask a new instance of makeCacheMatrix 
    #     -> feed x with the new matrix
    #     -> reset inv value 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # _get return the value of matrix
    get <- function() x
    
    # _setinv store the inverse value in cache
    setinv <- function(inverse) inv <<- inverse
    
    # _getinv get the inverse value stored in cache
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

## Write a short comment describing this function
## _CacheSolve : Check the presence of inverse, compute and save if not, 
## return the cache otherwise

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    # We try to get the inverse stored value  
    inv <- x$getinv()
    
    # If the an inverse value is found 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
	# If not compute and save the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Test cases
##  1_ 10x10 matrix
x <- matrix(rnorm(100),10,10)
y <- makeCacheMatrix(x)
##  Return result with computation of solve(x)
cacheSolve(y)
##  Return result from cache
cacheSolve(y)

##  2_ 6x6 matrix
x1 <- matrix(rnorm(36),6,6)
y1 <- makeCacheMatrix(x1)
##  Return result with computation of solve(x)
cacheSolve(y1)
##  Return result from cache
cacheSolve(y1)
