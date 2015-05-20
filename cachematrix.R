## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve work together to provide
## inverse matrixes, but only solving when not available in cache

## makeCacheMatrix establishes get and set functions for the matrix
## and its inverse, setting passed arguments to cache as needed
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  setInv<- function(calcInv) inv<<- calcInv
  getInv<- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)  
}


## Write a short comment describing this function
## cacheSolve will return inverse from cache if the x$inv is not null,
## else it will calculate inverse manually and set the value into cache and return inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    ## inverse is in cache
    message("getting cached inverse")
    return(inv)
  }
  ## inverse isn't in cache, need to calculate
  a<- x$get()
  inv<- solve(a)
  ## set inverse back in cache
  x$setInv(inv)
  return(inv)
}