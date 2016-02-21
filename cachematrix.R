## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
## The following two functions cache the inverse of a matrix, but also check to see if it exists first.

## makeCacheMatrix creates a list to create a function that:
## 1. Sets the value of the Matrix
## 2. Gets the value of the Matrix
## 3. Sets the value of the inverse of the Matrix
## 4. Gets the value of the inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## set initial value of inv to NUL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function returns the inverse of the matrix, by first checking if it has already been calculated
## if it has then it uses the stored matrix, else it will compute it (using the solve function), and then assigning it to
## the matrix.
## THERE IS THE ASSUMPTION THAT THE MATRIX IS ALWAYS INVERTABLE!!!

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        message("ALREADY CACHED - GET CACHED DATA")
        return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## TEST
## > x <- matrix(1:4, nrow = 2, ncol = 2)
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > 
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## ALREADY CACHED - GET CACHED DATA
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
