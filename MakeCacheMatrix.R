
# makeCacheMatrix function tests and create a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
      )
}

# cacheSolve function returns inverse of the matrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Getting cached matrix inverse")
    return(inv)
  }
  
  data_matrix <- x$get()
  inv <- solve(data_matrix, ...)
  x$setinverse(inv)
  
  inv
}

# Test script for MakeCacheMatrix.R

# Approach 1: create a matrix object, then use it as input to cacheSolve()

a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
cacheSolve(a)

# Call cacheSolve(a) a second time to trigger the "Getting cached matrix inverse" message
cacheSolve(a)

# Multiply the matrix by inverse, resulting in identity matrix
a$get() %*% a$getinverse()

# Reset a with another matrix to clear out cached value
a$set(matrix(c(2,3,2,2),2,2))

# Confirm that a has new data and that cache is NULL
a$get()

a$getinverse()

# Rerun cache solve, note that "Getting cached matrix inverse" does not print,
# and that we get a different result
cacheSolve(a)

# Approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
#             note that the argument to cacheSolve() is a different object
#             than the argument to the first call of cacheSolve()
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))

# Try a non-invertible matrix
b <- makeCacheMatrix(matrix(c(0,0,0,0),2,2))
cacheSolve(b)
