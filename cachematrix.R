

## Creating a special "matrix" that can catche its inverse
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
     x <<- y
     inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# cacheSolve function returns the inverse of the matrix.
# If the inverse is already computed and matrix is not canged the cached inverse is returned.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
   message("getting cached data.")
   return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
