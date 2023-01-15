## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) z <<- inverse
        getInv <- function() z
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## Computes the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not changed)
## then the "cacheSolve" function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInv()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data) %*% data
        x$setInv(z)
        z
}
