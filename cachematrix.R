## This pair of functions caches a matrix, solves the inverse, and then caches the solution.

## The makeCacheMatrix function takes the inverse of the matrix and caches it for future use. 
#It does this by caching a copied version of the matrix so that it can in
#turn cache the inverse of the matrix. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #creating an empty variable
  set <- function(y) {
    x <<- y #x is y in the local environment
    m <<- NULL #we have to initilaize variables for every function
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#Testing whether the function works
mymatrix<-makeCacheMatrix(rbind(c(1,0.25),c(0.25,1)))
mymatrix$get()
mymatrix$getinverse()

#cacheSolve takes the inverse of the cached matrix and then caches that inverse
cacheSolve<- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { # If m is anything other than empty
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)#
  x$setinverse(m)
  m
}
#Testing whether the function works
cacheSolve(mymatrix)
