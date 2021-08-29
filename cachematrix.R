makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Comment: This function (cacheSolve) works only when makeCacheMatrix exists. First, cacheSolve function checks if inverse of matric
## already exists (if m already has a value, for example: if user has inserted value or it was calculated previously, 
## it is taken from cached data). If not, it starts do find inverse of matrix, taking matrix values from the 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matr <- x$get()
        m <- solve(matr, ...)
        x$setinverse(m)
        m
}