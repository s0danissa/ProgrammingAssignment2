## Put comments here that give an overall description of what your
## functions do

## This is the function that creates a special type of 
## "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set_x <- function(a){
    x<<-a
    x_inv<<-NULL
  }
  
  get_x <- function() x
  setInv <- function() inv_x <<- solve(x)
  getInv <- function() inv_x
  
  list(set_x = set_x,
       get_x = get_x,
       setInv= setInv,
       getInv = getInv)
}


## This function computes the inverse of a matrix returned by the
## function 'makeCacheMatrix' and caches it. If the inverse has
## been already calculated the it returns the cached inverse of
## the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInv()
  if (!is.null(x_inv)){
    message("Getting cached data.")
    return(x_inv)
  }
  x_data <- x$get_x()
  x_inv <- solve(x_data, ...)
  x$setInv(x_inv)
  x_inv
}
