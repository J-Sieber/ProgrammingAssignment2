## This function creates a special matrix object that can cache its inverse

## This was done for credit for Coursera's R Programming class

## J-Sieber 4/27/2014

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    ## set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get function
    get <- function() x
    ## setInv function
    setInv <- function(solve) m <<- solve\
    ## getInv function
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## This function computes the inverse of the special matrix returned
## by makeCacheMatrix above.  If the inverse has already been calculated
## and the matrix has not changed then the cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    ## Determines if m is cached already
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if m has not been cached
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
        
}
