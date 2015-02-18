## Caching the Inverse of a Matrix
## cachematix.R contains two functions that cache the inverse of a matrix
## Author - Gitid: vsuo; email: vsuo@outlook.com

## This function creates a special "matrix" object that can cache its inverse
## using '<<-' operator which can be used to assign a value to an object in an 
## environment that is different from the current environment.
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    ## this function set the matrix 
    set <- function(y)
    {
        x<<- y
        m<<-NULL
    }
    ## this function return the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setinverse <- function (solve) m <<-solve
    
    ## get the inverse matrix variable
    getinverse <- function() m
    
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve 
##  the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## check for null before returing the cache inverse matrix
    if (!is.null(m))
    {
        message("return previously cached matrix")
        return(m)
    }
    
    ## get the matrix
    data <-x$get()
    
    ## perform the inversion using the solve()
    m <- solve(data, ...)
    
    ## call set inverse () to store the inverse of matrix
    x$setinverse(m)
    
    ## return the inverse matrix
    m
}
