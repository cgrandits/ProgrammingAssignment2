## a function to create a matrix with a cacheable inverse
## and another function to make use that cache feature

## creates a special matrix object that is able to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedinv <- NULL
    
    set <- function(y) {
        x <<- y
        cachedinv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) cachedinv <<- inverse
    
    getinverse <- function() cachedinv
    
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse )
}


## gets the inverse of a special matrix object, made from makeCacheMatrix
## if value is already cached, uses that; if not will solve and 
## cache value for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat,...)
    x$setinverse(i)
    i
}
