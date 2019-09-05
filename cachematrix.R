## Put comments here that give an overall description of what your
## functions do
#This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
#makes a cache matrix from a given matrix

makeCacheMatrix <- function(x = matrix()) {
  # 1. initialize the cache Matrix 'cacheMatrix'
  CacheMatrix <- NULL
  # 2. define the method named 'setMatrix'
  setMatrix <- function(y) {
    x <<- y
    CacheMatrix <<- NULL
  }
  #3. define the method named 'getMatrix' return the matrix 'x'
  getMatrix <- function() x  
  # 4. define the method named 'setCache' and 'getCache
  setCache <- function(inverse) CacheMatrix <<- inverse
  getCache <- function () CacheMatrix
  # 5. list the names of all methods that will be known to the outside world
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setCache= setCache,
       getCache  = getCache )
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # check the content of cache matrix
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached Matix")
    return(m)
  } else {
    # else the content is empty then: 
    # get the matrix, solve, set, update and return the cache matrix
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setCache(m)
    return (m)
  }
}

aSquareMatrix <- makeCacheMatrix(matrix(0:8, 8, 8))
aSquareMatrix$getMatrix()

aSquareMatrix$getCache()
cacheSolve(aSquareMatrix)
