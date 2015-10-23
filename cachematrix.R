## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## The cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
 
## Write a short comment describing this function

## makeCacheMatrix creates a matrix that can get & set value of the matrix and the inverse 
## The super assignment operator << makes it posible to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve first checks if the inverse matrix is calculated; if yes gets the value from cache 
## else gets the inverse of the matrix and sets the value with setinverse method in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
		
}
