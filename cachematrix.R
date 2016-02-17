## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function will create a cachable object, where, 
# a) we can set the value of a matrix, 
# b) get the value of the matrix,
# c) set the inverse of the saved matrix
# d) retrieve the inverse of the saved matrix,

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    ##set the value of the matrix
    set <- function(y)
    {
        x <<- y
        invx <<- NULL
    }
    ##retrieving the value of the matrix
    get <- function() x
    
    ##caching the matrix inverse
    setinv <- function(inv) invx <<- inv
    
    ##retrieving the matrix inverse
    getinv <- function() invx
    
    ##creating the list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}
#Example1
# the following command will create an object, having a 10X10 matrix
# > x <- makeCacheMatrix(matrix(rnorm(100),10,10))

#Example2
# to set the matrix
# > x$set()

#Example3
# to retrieve the matrix value,
# > x$get()



## Write a short comment describing this function
#The following function will take input an object, created by makeCacheMatrix() function,
# and a) will return the inverse of the matrix, stored in the object, if the inverse was cached,
# or, b) calculate the inverse of the stored matrix, if it the inverse was not cached, and at the end,
# it will return the calculated inverse of the matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinv()
    if(!is.null(invx))
    {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinv(invx)
    x$getinv()
}

#Example4
##getting the matrix inverse
# > cacheSolve(x)
#where, the object x was created as explained in Example1