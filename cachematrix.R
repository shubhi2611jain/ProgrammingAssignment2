## Cache the inverse of a matrix
## Rather than computinh a matrix repeatedly as it turns out to be an extremely costly computation
##I cache the inverse of matrix using these two functions

## makeCacheMatrix() is a function that creates a matrix object which can and will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve() is a function that computes/solves the inverse of the matrix , created by makeCacheMatrix() 
##In case of, the inverse has already been computed , 
##then it must retrieve/attain the inverse , from the cache .

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("now getting cached data")
    return(inver)
  }
  matar <- x$get()
  inver <- solve(matar...)
  x$setInverse(inver)
  inver      
}
