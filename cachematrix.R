## There are two functions to make an inverse matrix

## makeCacheMatrix makes a list of 4 functions. 
## This function stores cache of inverse matrix, 
## but it doesn't calculate inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(sol) inv <<- sol
        getinverse <- function() inv
        list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}

## This function first checks if makeCacheMatrix function has a cache of inverse matrix.
## If makeCacheMatrix does not have the cache,
## cacheSolve calculates inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
