make_cacheMatrix <- function(x = matrix()) { ## substituting matrix instead of mean in the orginal function
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve ## substituting solve instead of mean in the original function
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
---------------
  cache_Solve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)) { ## check to see if cache_Solve has been run before
      message("getting cached data")
      return(m)
    }
    matrix <- x$get() ## run the getmatrix function to get the value of the input matrix
    m <- solve(matrix, ...) ## compute the value of the inverse of the input matrix
    x$setmatrix(m) ## run the setmatrix function on the input matrix to cache it
    m ## return the inverse
  }
