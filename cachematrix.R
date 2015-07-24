#This couple of functions were made to cache the inverse 
#of a matrix

#This first function creates a "matrix" which really contains a list of functions
# to set and get a determined matrix, and also to set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
  set <- function (y){
    x<<- y
    inv<<- NULL
  }
  get <- function () x
  setinv <- function (inverse) inv<<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this functions calculates the inverse of a matrix a set it if it hasn't been calculated yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
  if (!is.null(inv)){
    message ("getting cached data")
    return (inv)
    
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
