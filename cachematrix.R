#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
#of a matrix rather than computing it repeatedly

##Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #Initializing the inverse
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Returns the matrix
  getMatrix <- function(){
    x
  }
  #the value of the argument passed is assigned as the inverse of the matrix x.
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  #Returns the inverse
  getInverse <- function(){
    inv
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}


##  computes the inverse of the special "matrix" returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  #Gets the value of inverse that was stored in the makeCacheMatrix function.
  inv <- x$getInverse()
  #If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matr <- x$getMatrix()
  #solve() calculates the inverse of the matrix passed
  #It's assumed here that the matrix passed is always invertible.
  inv <- solve(matr, ...)
  x$setInverse(inv)
  #Returns a matrix that's inverse of matrix passed.
  inv
}

#Sample test case
#matr_fn <- makeCacheMatrix(matrix(1:4,2,2))
#matr_fn$getMatrix()
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#matr_inv <-cacheSolve(matr_fn)
#matr_inv
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
