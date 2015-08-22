
## Caching the Inverse of a Matrix

## The first function "makeCacheMatrix" creates a special "matrix" and a "list" which contains:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseofmatrix <- NULL
  # set the value of the matix
  set <- function(y) {
    x <<- y
    inverseofmatrix <<- NULL
  }
  # get the value of the matix
  get <- function() x
  # set the value of the inverse of the matrix
  setinverse <- function(inverse) inverseofmatrix <<- inverse
  # get the value of the inverse of the matrix
  getinverse <- function() inverseofmatrix
  # list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix" created with the function "makeCacheMatrix". 
## But it first checks if the inverse of the matrix has already been computed. 
## If so, it gets the output from the cache and skips the computation. 
## Else, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inverseofmatrix <- x$getinverse()
  # Control:
  # If the inverse of the matrix has already been computed, it gets the output from the cache and skips the computation.
  if(!is.null(inverseofmatrix)) {
    message("Getting cached data.")
    return(inverseofmatrix)
  }
  # Else, function calculates the inverse of the matrix and sets the value in the cache via the setinverse function.
  data <- x$get()
  # solve () = Inverse of A where A is a square matrix.
  inverseofmatrix <- solve(data, ...)
  x$setinverse(inverseofmatrix)
  # output
  inverseofmatrix
}


## Output

## Create a matrix
## x <- matrix(1:4,2,2)
## x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## Cache the matrix
## y <- makeCacheMatrix(x) y$get()

## 1. Run (with no cache)
## cacheSolve(y)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## If you want to compare the result 
#  solve (x)

## 2. Run (with cache)
## cacheSolve(y)
## Getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
