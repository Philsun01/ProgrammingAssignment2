## Objective: create 2 functions that will cache the inverse of a matrix
## 1. makeMatrix creates special matrix that cache its inverse.  It will set and get the inputted matrix and
## set and get the inverse of the inputted matrix

## 2. cacheSolve computes the inverse of the special matrix.  It will check if the special matrix is not empty.
## If the special matrix has content, the fucntion will return this special matrix as the inverse with the message
## indicating it is a cached matrix.  Otherwise, compute the inverse of the inputted matrix.


## 1. makeMatrix creates special matrix that cache its inverse
makeMatrix <- function(x = matrix(nrow=2, ncol=2)) {
    invm <- matrix(nrow=2, ncol=2)
    set <- function(y) {
        x <<- y
        invm <<- matrix(nrow=2, ncol=2)
    }
    get <- function() x
    setinverse <- function(solve) invm <<- solve
    getinverse <- function() invm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 2. cacheSolve computes the inverse of the special matrix.
cacheSolve <- function(x, ...) {
    invm <- x$getinverse()
    if(!(all(is.na(invm)))) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinverse(invm)
    invm
}
