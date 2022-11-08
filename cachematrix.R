## Put comments here that give an overall description of what your
## functions do

## this function will initiate the matrix's value and all the 4 associated functions 

makeCacheMatrix <- function(matrix = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    matrix <<- y
    matrix_inverse <<- NULL
  }
  get <- function() matrix
  setinverse <- function(matr) matrix_inverse <<- matr
  getinverse <- function() matrix_inverse
  l=list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  return (l)
}



## This function will compute the inverse if its not available in cache, and return the cached result otherwise



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  matr<- solve(matrix)
  x$setinverse(matr)
  return (matr)
}
