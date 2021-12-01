## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      invM <- NULL
      set <- function(y) {
            x <<- y
            invM <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) invM <<- inverse
      getinv <- function() invM
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invM <- x$getinv()
      if(!is.null(invM)) {
            message("cached inverse matrix found, getting the matrix...")
            return(invM)
      }
      dades <- x$get()
      invM <- solve(dades, ...)
      x$setinv(invM)
      invM
}
