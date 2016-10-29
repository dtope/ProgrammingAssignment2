###HERE'S MY CODE FOR WEEK 3 - Programming Assignment 2

CacheMatrix <- function(x = matrix()) {
  
  ## create a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(z) {
    # use `<<-` to assign a value to an R object in an environment 
    # that is different from the current environment. 
    x <<- z
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

solvecache <- function(x, ...) {
  ## get output of CacheMatrix()
  ## return the inverse of the original matrix input to CacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get info from the cache and skip the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # generates the value of the inverse in the cache with the setinv function.
  x$setinv(inv)
  
  return(inv)
}
