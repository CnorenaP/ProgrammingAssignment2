
## Date: 2015/02/20
## Autor: Claudia Noreña
## This assignment has a pair of functions that cache and calculates the inverse of a matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse, where you can:
## set the value of the MATRIX
## get the value of the MATRIX
## set the value of the solve
## get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
	m3 <- NULL
    set1 <- function(y1) {
                x <<- y1
                m3 <<- NULL
        }
        get1 <- function() x
        setinv <- function(solve) m3 <<- solve
        getinv <- function() m3
        list(set1 = set1, get1 = get1,
             setinv = setinv,
             getinv = getinv)
}


## Function cacheSolve calculates the  inverse of the special "matrix" returned by makeCacheMatrix.
## It first checks if the matrix has not changed, if so, it will take the changed Matrix
## for that reason the parameter "matrix_i" is used to pass the current state of the matrix
## then verify if the inverse has already been calculated. If so, it gets the inverse matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, matrix_i, ...) {
        m3 <- x$getinv()
        datos <- x$get1()
        if(!identical(datos,matrix_i)) {
                m3 <- NULL
                message("getting modified data")
                datos <- matrix_i
        }
       
        if(!is.null(m3)) {
                message("getting cached data")
                return(m3)
        }
        m3 <- solve(datos, ...)
        x$setinv(m3)
        m3
}

