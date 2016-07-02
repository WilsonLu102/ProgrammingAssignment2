# This function creates a matrix used for calucating the inverse of itself
# This function has the four functionalities 
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

## Return a matrix that is the inverse of 'x'
## Method assumes that the matrix is inversible 
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    ## if inverse of matrix was  previously set by the user, retrieve this information
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## solve is the function to inverse a matrix
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
