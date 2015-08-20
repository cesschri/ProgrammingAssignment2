##########################################################################################################################
## Course.: R Programming class
## Pupose.: Programming Assignment 2
## Author.: cesschri
## Date...: August 19, 2015
##
## Description:
## Using the 2 stubbed out functions provided: makeCacheMatrix and cacheSolve, code them to demonstrate
## how an R function is able to cache potentially time-consuming computations. In this case we will "cache" 
## a matrix and it's solved inverse using the <<- assignment operator. This assignment teaches the scoping 
## rules of the R language and how they can be manipulated to preserve state inside of an R object.
## See the individual function descriptions below.
##
## NOTE: This code is written based on the requirements provided. I understand it to be a learning exercise and 
## not an example of ideal objected oriented programming.
##
## To test this assignment, you can run the following commands...
## x <- makeCacheMatrix()          # set a variable with a call to makeCacheMatrix()
## x$set(matrix(1:4,2,2))          # set the original matrix to values that produce a square matrix
## x$get()                         # display the original matrix
## cacheSolve(x)                   # set the inverse matrix and display it
## cacheSolve(x)                   # get the inverse matrix from cache and display it. Notice the message.
## x$get() %*% cacheSolve(x)       # FYI, the original matrix multiplied by it's inverse should be the identity matrix
##
## x$set(matrix(c(2, 2, 2, 1, 0, 0, 0, 0, 1),3,3))   # another square matrix example to test if desired
##########################################################################################################################

##########################################################################################################################
## FUNCTION: makeCacheMatrix
## This function sets a special matrix and it can cache its inverse.
## It cache's a matrix (x), which must be square, and also it's inverse (x_inverse) in the functions environment.
## It contains functions to set and get x. It also has functions to setinverse, and getinverse (the x_inverse
## matrix).
##########################################################################################################################
makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL                                       # "cached" inverse matrix variable
    
    set <- function(y) {
        x <<- y                                             # set the x matrix to the y matrix passed in
        x_inverse <<- NULL                                  # set the inverse matrix variable to NULL
    }
    
    get <- function() x                                     # return the original matrix x
    
    setinverse <- function(inverse) x_inverse <<- inverse   # set the x_inverse inverse matrix variable to passed in matrix
    
    getinverse <- function() x_inverse                      # return the cached inverse matrix
    
    # list of functions defined
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##########################################################################################################################
## FUNCTION: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above and caches it.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
## It always Returns the inverse matrix, but only displays the message if it was retrieved from cache.
##########################################################################################################################
cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()                     # get the inverse matrix from makeCacheMatrix into i
    
    if (!is.null(i)) {                      # if i is not null then display the message that it was
        message("getting cached data")      # pulled from the cache.
        return(i)                           # retrun the inverse matrix and end execution
    }
    
    # if the inverse matrix does not exist in the cache, the following code is executed...
    data <- x$get()                         # get the original matrix and assign to data variable
    
    i <- solve(data)                        # Compute the inverse of the matrix "data".
                                            # data is assumed to be a square matrix.
    
    x$setinverse(i)                         # Set the cache variable
    i                                       # return the inverse matrix
}
