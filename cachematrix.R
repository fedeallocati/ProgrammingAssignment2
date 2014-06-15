## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions
## that are used to wrap the memorization
makeCacheMatrix <- function(x = matrix()) 
{
    # 'i' stores the cached inverse
    # It starts NULL
    i <- NULL
    
    # set modifies the current matrix and
    # erases previously saved inverse
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
    
    # get returns the current matrix
    get <- function() x
    
    # setinverse saves the parameter 'inverse'
    # into the 'i' variable
    setinverse <- function(inverse) i <<- inverse
    
    # getinverse returns the cached inverse
    getinverse <- function() i
    
    # return a list object with the 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix created
## by the makeCacheMatrix function.
## It first checks if the cached inverse isn't null, 
## and if it isn't, returns it. 
## If it is null, it calculates the inverse, saves it 
## in the cached object and returns it.
cacheSolve <- function(x, ...) 
{    
    i <- x$getinverse()
    
    ## Check if the inverse was already calculated
    if(!is.null(i)) 
    {
        message("getting cached data")
        
        ## Return cached inverse
        return(i)
    }
    
    # Get the matrix
    data <- x$get()
    
    # Calculate the inverse with 'solve'
    i <- solve(data, ...)
    
    # Save the inverse into the cache object
    x$setinverse(i)
    
    # Return calculated value
    i
}
