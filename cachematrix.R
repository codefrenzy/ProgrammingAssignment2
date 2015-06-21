## Title: R Programming - Caching the Inverse of a Matrix
## Author: Annie Luxton
## Description: Since calculating the inverse of a matrix can be computationally expensive, 
## we can calculate it once and cache it, until the matrix itself changes.

## makeCacheMatrix: Returns a list of functions that perform the following:
## 1. set the value of the matrix and null out the value of the inverse of the matrix
## because it will have to be recalculated for the new value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Returns the inverse of the matrix. First it checks to see if the inverse
## has already been calculated for the current matrix value and if so, it returns the result
## without recalculating it. If not, it gets the current matrix value via the get function, 
## calculates the inverse, sets the value in the cache via the setInverse function and 
## returns the inverse matrix.

## cacheSolve assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}

## Example:
## > x <- matrix(1:4, ncol = 2, nrow = 2)
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## The inverse matrix has not yet been calculated
## > m$getInverse()
## NULL

## The inverse matrix is not cached yet so cacheSolve will calculate it
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Now the inverse matrix can be returned
## > m$getInverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Since the inverse matrix is already cached, it won't recalculate it on the second run 
## > cacheSolve(m)
## Getting cached inverse matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## If we change the matrix, the inverse matrix is also cleared from the cache
## > x <- matrix(11:14, ncol = 2, nrow = 2)
## > m$set(x)
## > m$getInverse()
## NULL

## The inverse matrix needs to be calculated and put into cache again
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5

## The inverse matrix is returned from cache again
## > m$getInverse()
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5

## Now the inverse matrix for the new matrix is in cache again, it doesn't neeed to be 
## recalculated
## > cacheSolve(m)
## Getting cached inverse matrix
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5