## Put comments here that give an overall description of what your
## functions do

## creates a special matrix that can cache its reverse. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    ## When changing the value of the matrix, the cache 'i' is evicted.
    i <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}
## returns the inverse of x from cache if it exists, compute it otherwise.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
          message("using cached data")
          return(i)
        }
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
