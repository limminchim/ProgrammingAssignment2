## Put comments here that give an overall description of what your
## functions do

## Example: Caching the Mean of a Vector
## The <<- operator 
##  - can be used to assign a value to an object in an environment
##    that is different from the current environment. 
## Below are 2 functions used to create a special object that stores a numeric vector and cache its mean. 


## Creates a special "vector", which is really a list containing a function to
##    - set the value of the vector
##    - get the value of the vector
##    - set the value of the mean
##    - get the value of the mean
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## Calculates the mean of the special "vector" created with the above function. 
## - First checks to see if the mean has already been calculated. 
## - If yes, it gets the mean from the cache and skips the computation. 
## - If no, it calculates the mean of the data and sets the value of the mean 
##      in the cache via the setmean function.
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


## Write a short comment describing this function
## R function to cache potentially time-consuming computations
## Creates a special "matrix", which is really a list containing a function to
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the value of the inverse matrix
##    - get the value of the inverse matrix
## Assumes x is a invertible square matrixca
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    


}

## Write a short comment describing this function
## R function to Calculates the inverse of a matrix
## Calculates the inverse of the special matrix created with the above function. 
## Assumes it is an invertible matrix
## - First checks to see if the inverse has already been calculated. 
## - If yes, it gets the inverse from the cache and skips the computation. 
## - If no, it calculates the inverse of the matrix and sets the value of the inverse 
##      in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data of inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m        
}
