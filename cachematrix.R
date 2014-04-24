## List of functions, where you can set or get a matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {     ##function set: create a matrix
    x <<- y
    inv <<- NULL           ##give the inverse variable NULL value
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse  
  ##set the inverse and store in cache
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a matrix, checks if the inverse is stored in cache. If it is not, it calculates the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()     ##using makeCacheMatrix, try to get the inverse
  if(!is.null(inv)) {       ##if inverse matrix is in cache,
                            ##gets the value and inform is getting the cache 
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)    ##if is not in the cache, solve() calculates the inverse 
  x$setinverse(inv)         ##now the inverse is stored in cache using setinverse()
  inv
}
