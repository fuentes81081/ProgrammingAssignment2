## This function creates a special "matrix" object that can cache its inverse.
## see cacheSolve()

makeCacheMatrix <- function(theMatrix = matrix()) {
    if(!exists("cachedMatrix")) {
        cachedMatrix <<- NULL
    }

    get <- function() {
        theMatrix
    }

    setInverse <- function(inverse) {
        cachedInverse <<- inverse
    }

    getInverse <- function() {
        if(!identical(theMatrix, cachedMatrix)) {
            cachedInverse <<- NULL
            cachedMatrix  <<- theMatrix
        }
        cachedInverse
    }

    list(get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.
## Usage: cacheSolve(makeCacheMatrix(myMatrix))

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!exists("cachedMatrix")) {
        cachedMatrix <<- NULL
    }

    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    importedData <- x$get()
    inverse <- solve(importedData)
    x$setInverse(inverse)
    inverse
}

## This function creates a randomized matrix with x by x dimension
## Usage: myMatrix <- createMatrix()

createMatrix <- function(x = 2) {
    output <- matrix(runif(x*x), ncol=x)
}




## myMatrix <- createMatrix()
## cacheSolve(makeCacheMatrix(myMatrix))
