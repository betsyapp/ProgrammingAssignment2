## get the inverse of a matrix by computing it or from a cache if already cached

## the first function does the following:
## sets the value of the matrix
## gets the value of the matrix
## sets the inverse of the matrix
## gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## the second function calculates the inverse of the matrix created with the above function.
## first checks to see if the inverse has already been calculated.
## if so, it gets the inverse from the cache and skips the computation.
## otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
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

## test using a sample matrix
mymatrix <- matrix(data = c(1,3,2,5), nrow = 2, ncol = 2) #create a 2 by 2 matrix
mymatrix #check it out
myinverse <- solve(mymatrix) #compute its inverse
myinverse #check it out...this is what the cacheSolve function should return

speciallist <- makeCacheMatrix(mymatrix)
cacheSolve(speciallist)
