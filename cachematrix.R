## cachematrix.R
## These two functions work together to repeatably return the inverse of a 
## matrix while only having to do the actual inversion once.
##
## Usage: cacheSolve(makeCacheMatrix(A_df))
##        where A_df is a matrix converted to a recursive data frame
##
## Example
## Create an invertable matrix
## A <- matrix( c(5, 1, 0,
##                3,-1, 2,
##                4, 0,-1), nrow=3, byrow=TRUE)
## 
## A_df <- data.frame(A)
##
## cacheSolve(makeCacheMatrix(A_df))
## [,1]    [,2]   [,3]
## X1 0.0625  0.0625  0.125
## X2 0.6875 -0.3125 -0.625
## X3 0.2500  0.2500 -0.500
##
## Check using solve()
## solve(A_df)
## [,1]    [,2]   [,3]
## X1 0.0625  0.0625  0.125
## X2 0.6875 -0.3125 -0.625
## X3 0.2500  0.2500 -0.500
##
## makeCacheMatrix creates a placeholder for a matrix then creates a function 
## containing the steps to calculate the inverse and cache it OR 
## make use of a cached inverse 

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


## cacheSolve returns the inverse of the matrix provided by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
} 


