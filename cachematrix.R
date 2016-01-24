#This function allows the matrix to be replaced by the
#input y.The solve function is being used here to compute
#the inverse of a square matrix and list allows the four functions 
#within makeCacheMatrix to be used within the makeCacheMatrix function.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL   
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The purpose of this function is to first check if the value m which had 
## been stored by getmatrix exists and is not null with setmatrix used to allow
## cacheSolve to return the inverse of matrix x to the console.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}