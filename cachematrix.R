## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() {x}
  setIv <- function(inv) {iv <<- inv}
  getIv <- function() {iv}
  list(set = set, get = get, setIv = setIv, getIv = getIv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  iv <- x$getIv()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  mat <- x$get()
  iv <- solve(mat, ...)
  x$setIv(iv)
  iv
}