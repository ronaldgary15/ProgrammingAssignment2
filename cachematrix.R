## cachematrix.R created 3-16-2015 by RonaldGary
## 
## mackecacheMatrix() function caches a provided matrix as 'x'
## additionally allows for storage and retrieval of inverse matrix.
##
## cacheSolve() function that check if inverse has been calculated and cached 
## or calculate the inverse and store to cache.

## function to store magrix as global variable m
makecacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y){                 # allows you to replace the cached matrix
                x <<- y                     #  stores new matrix passed by x$set(<MATRIX>)
                inverse_x <<- NULL 
        }
        get <- function () x                # Retrieves matrix stored with x$get()
        setinverse <- function(inverse) inverse_x <<- inverse # Caches inverse value
        getinverse <- function() inverse_x  # retrieve inverse value
        list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## function to retrieve cached matrix and provide inverse of 'x'
cacheSolve <- function(x, ...) {
        cached_inv <- x$getinverse()  # retrieves inverse
        if (!is.null(cached_inv)) {   # if cached inverse exists, return cached_inv
                message("getting cached inverse")
                return(cached_inv)
        } else {                      # if cached inverse does not exist calculate new inverse
                solved_inv <- solve(x$get())  # Calculation to create inverse
                x$setinverse(solved_inv)      # save inverse in cache
                return(solved_inv)
        }
}

