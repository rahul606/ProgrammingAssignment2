# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setCacheMatrix      set the value of a matrix
# * getCacheMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getCacheMatrixInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
    
    # holds the cached value or NULL if nothing is cached
    # initially nothing is cached so set it to NULL
    cacheMatrix <- NULL
    
    # store a matrix
    setCacheMatrix <- function(newValue) {
        x <<- newValue
        # since the matrix is assigned a new value, flush the cache
        cacheMatrix <<- NULL
    }
    
    # returns the stored matrix
    getCacheMatrix <- function() {
        x
    }
    
    # cache the given argument 
    cacheInverse <- function(solve) {
        cacheMatrix <<- solve
    }
    
    # get the cached value
    getCacheMatrixInverse <- function() {
        cacheMatrix
    }
    
    # return a list. Each named element of the list is a function
    list(setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix, cacheInverse = cacheInverse, getCacheMatrixInverse = getCacheMatrixInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
    # get the cached value
    inverse <- y$getCacheMatrixInverse()
    # if a cached value exists return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    data <- y$getCacheMatrix()
    inverse <- solve(data)
    y$getCacheMatrixInverse(inverse)
    
    # return the inverse
    inverse
}