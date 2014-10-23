## This is a R script that cache the inverse of an inversable matrix.
## It also compute the inverse of a matrix if R can't find it in the cache.

## This script creates a special matrix object that can cahche its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse matrix
    inv_x <- NULL
    
    ## Set the matrix
    set <- function(matrix){
        x <<- matrix
        inv_x <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Set the inverse of a matrix
    setInverse <- function(inverse) {
        inv_x <- inverse
    }
    
    ## Get the inverse of a matrix
    getInverse <- function() inv_x
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse
         )
}


## This script check whether the inverse of a matrix has been calculated.
## If the inverse already exists, the this function retrieves the inverse
## from the cache.  Otherwise it will compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getInverse()
    
    ## Check whether the inverse matrix is already set
    ## If it's set, then just return it
    if (!is.null(inv_x)) {
        message("Get cached data -- inverse of the matrix")
        return(inv_x)
    }
    
    ## Get the matrix object
    data <- x$get()
    
    ## Compute the inverse matrix
    inv_x <- solve(data,...) 
    
    ## Set the matrix inverse
    x$setInverse(inv_x)
    
    # Return the matrix inverse
    inv_x
}
