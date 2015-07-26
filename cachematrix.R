## Peer Assignment - Assignment 2 - R Programming

## This function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m   <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
get   <- function() x
setmatrix <- function(solve) m<<- solve
getmatrix <- function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## This function computes inverse of special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m      <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
}
