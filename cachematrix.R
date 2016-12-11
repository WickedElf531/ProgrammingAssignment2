## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function on a matrix 'x' which returns a 
## list of four sub-functions on the same matrix, 'x'.

makeCacheMatrix <- function(x = matrix()) {
  i_ <- NULL
  set <- function(y) {
    x <<- y
    i_ <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i_ <<- inv
  getinv <- function() i_
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
       
}


## cacheSolve is a function which checks to see if the inverse
## of a matrix 'x' has been calculated.
## If, so, it retrieves the matrix's inverse
## If not, it computes the matrix's inverse

cacheSolve <- function(x, ...) {
  i_ <- x$getinv()
  if(!is.null(i_)) {
    message("getting cached data")
    return(i_)
  }
  data <- x$get()
  i_ <- solve(data, ...)
  x$setinv(i_)
  i_
          ## Return a matrix that is the inverse of 'x'
}


