## The function "makeCacheMatrix" is a function which takes an argument x of type
## numeric vector and it returns a list with 4 list items 
## (they are actually 4 functions wrapped in a list)

## This function (makeCacheMatrix) return a list of 4 functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv){
  m <<- inv
  } 
  
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Another function "cacheSolve" is just a client function that uses "makeCacheMatrix"
## function in its implementation.The input is expecting a "special Matrix" made from
## makeCacheMatrix function. The output is the inverse coming whether from the special
## Matrix's  cache or computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <<- x$getinv()
        ## searching for cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## if there is cache return the inverse
  }
  data <- x$get()
  m <<- solve(data, ...)
  x$setinv(m)
  m
  ## if there is no cache, compute the inverse
}
