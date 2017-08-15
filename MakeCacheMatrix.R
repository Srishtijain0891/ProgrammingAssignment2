
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
