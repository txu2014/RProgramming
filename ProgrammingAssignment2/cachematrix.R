## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix contains 4 function, which get/set the matrix, and get/set the cache of its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    #use runtime dynamic scoping; 
    #otherwise by default R assign the values when creating the function
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)
        ## Return a matrix that is the inverse of 'x'
}
