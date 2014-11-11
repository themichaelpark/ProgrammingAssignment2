# makeCacheMatrix creates a list of functions that work together to store the solution from
# inverting a matrix. cacheSolve calculates the inverse of the matrix, if the solution hasn't 
# already been stored. It is assumed that any matrices used will be invertable, so no error
# detection methods have been put in place.

# makeCache Matrix creates a list containing a function to
# 
# 1.  set the value of a matrix
# 2.  get the value of a matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<-y
    m<<-NULL
  }
  get <- function() x
  setinv <- function (inverse) m <<- inverse
  getinv <- function () m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}

