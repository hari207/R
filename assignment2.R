#Introduction

#This 2nd programming assignment 
#will require you to write an R function that is able to cache potentially time-consuming computations. 
#For example, taking the mean of a numeric vector is typically a fast operation. 
#However, for a very long vector, it may take more time to compute the mean,  
#if it has to be computed repeatedly . 
#If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, 
#it can be looked up in the cache rather than recomputed. 
#In this Programming Assignment you will take advantage of the scoping rules of the R language 
#and how they can be manipulated to preserve state inside of an R object.

#  Example: Caching the Mean of a Vector



#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#
#    set the value 
#    get the value
#    set the value 
#    get the value 


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
#The first function, cacheSolve  

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

