## Inverse matrix computation and storage for future use, if input matrix
## is unchanged. All functions are thoroughly docummented to prove proper 
## understanding, due to the nature of this exercise, though that could
## be counter-productive in other circumstances.


## Function to be assigned to a variable so cacheSolve can make use of its
## nested functions.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Sets 'inv' to null, so it exists (but has no value) when 
    ## called by getinverse. Otherwise, an error would ocurr.
    inv <- NULL
    
    ## Function to store a new input matrix in cache, so we don't need to call
    ## again makeCacheMatrix. Also removes any possible previously computed
    ## inverse matrix (from previous input matrix).
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Function to return input matrix
    get <- function() x
    
    ## Function to store in cache inverse matrix on object inv.
    setinverse <- function(solvedinv) inv <<- solvedinv
    
    ## Function to retrieve stored matrix.
    getinverse <- function() inv
    
    ## Return a list will all previous functions when makeCacheMatrix is called.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of input matrix x, either computing
## it for the first time or getting it from cache. In order to run, 
## makeCacheMatrix must have been assigned to a variable before, which
## would be our input 'x' for cacheSolve.

cacheSolve <- function(x, ...) {
    
    ## Getting possible already computed inverse matrix.
    inv <- x$getinverse()
    
    ## Checking if inverse matrix has already been computed (has inv any value?)
    if(!is.null(inv)) {
        message("Getting inverse matrix from cache.")
        return(inv)
    }
    
    ## Gets input matrix and computes its inverse.
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Uses setinverse to store inv in cache; then returns inv.
    x$setinverse(inv)
    inv
}
