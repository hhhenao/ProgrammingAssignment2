makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

 ## It´s necessary to put the result of "makeCacheMatrix" in a new variable.
 ## and with this variable, run the "cacheSolve" function.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
        
}
