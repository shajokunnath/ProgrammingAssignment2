## First function is a list of 4 functions.  Each function works as follows.
##1. set the value of a matrix. 2. get the value of a matrix. 3. set the value of a inverse of matrix and 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

 
## Next funciton returns a matrix that is the inverse of 'x'if it can not be cached from the previous function.
cacheSolve <- function(x, ...) {
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
