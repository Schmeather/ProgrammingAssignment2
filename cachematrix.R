# The two functions below serve to reduce computation time 
# by caching the matrix inverse rather than re-computer an inverse 
# for a matrix that has not changed.

# The makeCacheMatrix function creates a list and sets the initial value.
# The intial value of the inverse is set to NULL.
# It then get and sets the value of the matrix and then
# gets and sets the vaule of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y){
      x <<- y
      inver <<- NULL
    }
      
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The function below calculates the inverse of the matrix in the function above.
# It first checks to see if the inverse of the matrix has already be calcuated
# in the function above.  If so, it returns that result and does not compute the inverse.
#  If not, it calculates the inverse and then places the value in the variable inver.

cacheSolve <- function(x, ...) {
    inver <- x$getinverse()
    if(!is.null(inver)) {
      message("getting cached inverse data")
      return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}
