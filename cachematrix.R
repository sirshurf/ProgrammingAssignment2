## 2 function that saves Matrix Inversions in cache and retreves it in need.

## This function holds the cache data of the matrix calculation 

makeCacheMatrix <- function(x = matrix()) {
  solverData <- NULL
  set <- function(y) {
    x <<- y
    solverData <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) solverData <<- solve
  getsolve <- function() solverData
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checkes if the Cache allready have this matrix, and if it has returnes it
## If the matrix inv is not existing in the cache, then calculates it, save it in cache and return it

cacheSolve <- function(x, ...) {
  solverData <- x$getsolve()
  if(!is.null(solverData)) {
    message("getting cached data")
    return(solverData)
  }
  data <- x$get()
  solverData <- solve(data, ...)
  x$setsolve(solverData)
  solverData
}
