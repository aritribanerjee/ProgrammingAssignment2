## The following functions can be used to create a special matrix object and
## calculate and cache its inverse

## This function creates a special matrix object which has four functions 
## that can be used to set and get the value of the matrix,
## and also to set and get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special matrix returned by the
## makeCacheMatrix function. It first checks whether the inverse has already
## been calculated, in which case it just retrieves it from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("fetching cached data")
    return(i)
  }
  mtx <- x$get()
  i <- solve(mtx, ...)
  x$setinverse(i)
  i
}

# Sample Run:
# > x <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 5), nrow = 3, ncol = 3)
# > m <- makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    1    2    4
# [2,]    1    3    4
# [3,]    2    3    5

# No cache present in first run, so inverse is calculated
# > cacheSolve(m)
# [,1]       [,2]       [,3]
# [1,]   -1 -0.6666667  1.3333333
# [2,]   -1  1.0000000  0.0000000
# [3,]    1 -0.3333333 -0.3333333

# Cache is retrieved from cache
# > cacheSolve(m)
# fetching cached data
# [,1]       [,2]       [,3]
# [1,]   -1 -0.6666667  1.3333333
# [2,]   -1  1.0000000  0.0000000
# [3,]    1 -0.3333333 -0.3333333
