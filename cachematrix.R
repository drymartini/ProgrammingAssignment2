## Coursera R-Language Programming Programming Assignment 2.

## Functions for a custom matrix that can cache its inverse.

## The constructor for a custom matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    return(x)
  }
  setInv <- function(i) {
    inv <<- i
  }
  getInv <- function() {
    return(inv)
  }
  return(
    list(
      set = set,
      get = get,
      setInv = setInv,
      getInv = getInv
      )
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  return(inv)
}


# Test function.
test1 <- function() {
  content = c(1.0, 0, 0,  0, 1.0, 0,  0, 0, 3.0 )
  orig <- makeCacheMatrix(matrix(content, 3, 3))
  origInv = cacheSolve(orig)
  print(orig$get())
  print(origInv)
}

# Uncomment the next line to run test.
# test1()
