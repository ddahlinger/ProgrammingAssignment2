## makeCacheMatrix function creates a square matrix object that can be inverted
## cacheSolve fuction computes the inverse of the matrix created
## by the makeCacheMatrix function

## function creates square matrix that can be inverted by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse of the matrix created in the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        ## Determines if the inverse calculation is in cache
        if (!is.null(inv)){
               
                message("loading the inverse calculation from cache")
                return(inv)
        }
        
        ## Calculate the inverse if it does not exist in cache
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        
        return(inv)
}
