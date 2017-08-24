## These functions cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set=set,get=get,setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x,...) {
      ## Return a matrix that is the inverse of 'x'
      a <- makeCacheMatrix(x)
      m <- a$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- a$get()
      m <- solve(data,...)
      a$setinverse(m)
      m
}