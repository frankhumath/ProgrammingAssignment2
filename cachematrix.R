##These two functions serves to cache the inverse of a given matrix

##The first function, makeCacheMatrix creates a special "matrix", which 
##is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
  
}


##The following function calculates the inverse of the special 
##"matrix" created with the above function. However, it first 
##checks to see if the inverse has already been calculated. If so, 
##it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the 
##value of the mean in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data, ...)
  x$set_inverse(m)
  m
}

