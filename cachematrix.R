# makeCacheMatrix stores the value of the "input_matrix" and compute 
# the inverse of the "input_matrix"
# as the input_matrix change, as long as cache with makeCacheMatrix,
# e.g. time1 <- makeCacheMatrix("input_matrix")
# CacheSolve(time1) returns the inverse of "input_matrix" at time1
# time1$get() returns the value of the "input_matrix"


# stores the "input_matrix" and compute the inverse of "input_matrix"
makeCacheMatrix <- function(mtrx = matrix()) {
  slvd <- NULL
  set <- function(y) {
    mtrx <<- y
    slvd <<- NULL
  }
  get <- function() mtrx
  setsolve <- function(solve) slvd <<- solve
  getsolve <- function() slvd
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)}

# returns the cached inverse matrix of "input_matrix"
CacheSolve <- function(mtrx, ...) {
  slvd <- mtrx$getsolve()
  if(!is.null(slvd)) {
    message("getting cached data")
    return(slvd)
  }
  data <- mtrx$get()
  slvd <- solve(data, ...)
  mtrx$setsolve(slvd)
  slvd
}
