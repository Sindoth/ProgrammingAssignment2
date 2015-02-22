# Function that Caches a matrix argument.
# Returns a vector of parameters for the matrix inverse calculation.

makeCacheMatrix <- function(x=matrix()){
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# Function that calculates the inverse of a square matrix, if the matrix was
# previously calculated then returns the inverse matrix from cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data) %*% data
        x$setsolve(s)
        s
}
       



