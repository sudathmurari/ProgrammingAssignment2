makeCacheMatrix <- function(x = matrix()) {
        
        # "cache" will hold the cached matrix
        # we shall set it to NULL initially
        cache <- NULL
        
        # function definition to store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }
        
        # function definition to fetch the matrix
        getMatrix <- function() {
                x
        }
        
        # function definition to cache the matrix
        setInverse <- function(solve) {
                cache <<- solve
        }
        
        # function definition to fetch the cached matrix
        getInverse <- function() {
                cache
        }
        
        # a list of functions is returned
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}



# Function to calculate the inverse of a matrix
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the inverse of y
        inverse <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # caclulate the inverse and store it in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$setInverse(inverse)
        
        # return inverse
        inverse
}