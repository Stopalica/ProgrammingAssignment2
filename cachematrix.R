makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmean <- function(inverse) inv <<- inverse
  getmean <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}



cacheSolve <- function(x, ...) {
  inv <- x$getmean()
  if(!is.null(inv)) {
    message("getting inv matrix")
    return(inv)
  }
  X <- x$get()
  inv <- solve(X)
  x$setmean(inv)
  inv
         ## Return a matrix that is the inverse of 'x'
}
