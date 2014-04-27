## makeCacheMatrix takes an invertible matrix & caches its value & inverse
## cacheSolve calculates the inverse of a matrix created via makeCacheMatrix 

## Creates a 'special matrix', ie. list of functions that: 
## 1. set value of matrix, 2. get value of matrix
## 3. set value of matrix inverse, 4. get value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Calculates the inverse of the matrix created by 'makeCacheMatrix'
## 1st checks to see if inverse has already been calculated
## if so, prints cached value & skips computaion
## if not, calcuates the inverse and sets it via setinverse function

cacheSolve <- function(x, ...) {
      
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data,...)
      x$setinverse(i)
      i
}
