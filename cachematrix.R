## The function below creates a special "matrix" containing
## a function to
## 1. set the value of the matrix (setMatrix)
## 2. get the value of the matrix (getMatrix)
## 3. set the inverse of the matrix (cacheInverse)
## 4. get the inverse of the matrix (getInverse)

makeCacheMatrix <- function(m = matrix()) {
cache <- NULL
setMatrix <- function(n) {
        m <<- n
        cache <<- NULL
}
getMatrix <- function() m

cacheInverse <- function(solve) cache <<- solve

getInverse <- function()  cache

list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        cacheInverse = cacheInverse,
        getInverse = getInverse)
}

## The inverse of the special "matrix" that was created with the above is calculated
## If the inverse has been calculated already, it should then be obtained from the cache.
cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'     
        inverse <- m$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- m$getMatrix()
        inverse <- solve(mat, ...)
        m$cacheInverse(inverse)
        
        inverse
}
