##This script will write two functions to 
## cache or store the inverse of a matrix.

## makeCachematrix is a function
## that creates a matrix object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <-NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function () x
  setInverse <- function (solve) inverse <<- solve
    getInverse <- function () inverse
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## The cacheSolve function computes
## the inverse of the matrix from the 
## makeCacheMatrix function. If the inverse
## is already calculated then it will retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...)  {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }

#create a matrix called 'm'
# with two rows and two
# columns
m <- matrix(c(-4, 3, 2, 1), c(2, 2))

#store this matrix with the
#makeCacheMatrix function
mm <- makeCacheMatrix(m)

#this would be the 
#first time we obtained the
#inverse of the cacheSolve
#function from 'mm'.
cacheSolve(mm)
# [,1] [,2]
# [1,] -0.1  0.2
# [2,]  0.3  0.4

#running it again would
#show that it's obtaining
#the values from the cached
#inverse instead of calculating it again.
cacheSolve(mm)
# getting cached inverse
# [,1] [,2]
# [1,] -0.1  0.2
# [2,]  0.3  0.4

mm$set(t(m) * 10) # matrix changes here
cacheSolve(mm)
# [,1] [,2]
# [1,] -0.01 0.03
# [2,]  0.02 0.04
cacheSolve(mm)
# getting cached inverse
# [,1] [,2]
# [1,] -0.01 0.03
# [2,]  0.02 0.04
