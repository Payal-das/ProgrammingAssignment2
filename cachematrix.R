## In this program, the concept of Lexical scoping in R is being implemented
## through caching inverse of a matrix. We have assumed that the matrix
## passed as argument is invertible. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
      inverse<-x$getinv()
      if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
      }
      data<-x$get()
      inverse<-solve(data)
      x$setinv(inverse)
      inverse
}
