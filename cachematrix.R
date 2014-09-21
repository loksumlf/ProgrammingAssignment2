#This assignment include a pair of functions that 
#cache the inverse of a matrix

#This function creates a special "matrix" object that can 
#cache its inverse
makeCacheMatrix <- function( x = matrix(nrow=0, ncol=0) ) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

#This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above
cacheSolve <- function(x) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
