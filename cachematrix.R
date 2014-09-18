## The <<- operator 
##  - can be used to assign a value to an object in an environment
##    that is different from the current environment. 
## Below are 2 functions used to create a special object that stores a special
##    matrix and cache its inverse. 

## R function to cache potentially time-consuming computations
makeCacheMatrix <- function(x = matrix()) {
        ## assumes x is a invertible square matrix

        ## Creates a special "matrix", which is really a list containing  
        ## functions to
        ##    - set the value of the matrix
        ##    - get the value of the matrix
        ##    - set the value of the inverse matrix
        ##    - get the value of the inverse matrix        
        
        ## Return a list of the form:
        ##    - function to set the value of the matrix
        ##    - function to get the value of the matrix
        ##    - function to set the value of the inverse matrix
        ##    - function to get the value of the inverse matrix  
      
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) m <<- solve
        
        getinverse <- function() m
        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)    
}


## R function to calculate the inverse of a matrix created with the 
## above function. 
cacheSolve <- function(x, ...) {
        ## x is a special "matrix" list returned from above function
        ## assumes x is an invertible square matrix
        
        ## - First checks to see if the inverse has already been calculated. 
        ## - If yes, it gets the inverse from the cache and skips the 
        ##      computation. 
        ## - If no, it calculates the inverse of the matrix and sets the value 
        ##      of the inverse in the cache via the setinverse function.
        
        ## Return a matrix that is the inverse of x
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data of inverse matrix")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m        
}
