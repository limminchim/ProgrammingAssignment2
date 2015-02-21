## Example: Caching the Mean of a Vector
## The <<- operator 
##  - can be used to assign a value to an object in an environment
##    that is different from the current environment. 
## Below are 2 functions used to create a special object that stores a numeric 
##    vector and cache its mean. 

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

