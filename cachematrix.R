#Below are two functions that are used to create
#a special object that stores a matrix and cache's its inverse.

#The first function, makeCacheMatrix  creates a special "matrix" object,
#that can cache its inverse.
#It is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setinv <- function(solve) inv <<- solve
  #get the value of the inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#The following function, cachesolve calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the matrix inverse
#has already been calculated. If so, it gets the mean from the cache and skips 
#the computation. Otherwise, it calculates the mean of the data and sets the 
#value of the mean in the cache via the setmean function.


cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}