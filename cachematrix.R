## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
