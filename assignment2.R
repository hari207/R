makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() {
    x
  }
  
  set_inverse <- function(inverse) {
    i <<- inverse
  }
  
  get_inverse <- function() {
    i
  }
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("Getting data..")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}

