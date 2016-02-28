## Below are two functions that are used to create a special matrix 
## that stores a numeric matrix and caches its inverse
## the concept is same like makeVector function but it is a matrix 
## with its inverse instead of vector with its mean



## this function creates a matrix and cache its inverse

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


## This function calculates inverse of matrix made above. However,
## if its inverse already calculated and cached then this function 
## will return the inverse from cache instead 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
