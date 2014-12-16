## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Funtion makeCacheMatrix takes a matrix m as an argument. It creates
## a varibale inv_m, where the inverse of the matrix m will be stored.
## Four functions are also defined within makeCacheMatrix - set, get, 
## set_inv and get_inv, which allow to set and get the original matrix m in
## cache, and set and get the inversed matrix correspondigly.
## When we call makeCacheMatrix, it creates an object, associated with 
## the input argument (standard matrix) and which stores the cache for the 
## inverse of the input argument, and provides functions to operate with it
##
makeCacheMatrix <- function(m = matrix()) {
        # inversed cached matrix
        
        # Initializing cashe for the inversed matrix
        inv_m <- NULL
        
        # To store the original matrix and initialize cache for it
        set <- function(y) {
                m <<- y
                inv_m <<- NULL
        }
        
        # To get the original matrix
        get <- function() m
        
        # To cache the inversed matrix
        set_inv <- function(inv) inv_m <<- inv 
        
        # To get cached inversed matrix
        get_inv <- function() inv_m
        
        # List of functions, provided
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)  
}

## Write a short comment describing this function
##
## Function cashSolve - takes a special matrix, created with makeCachMatrix
## funtion, as an argument. This function checks, if there is a cached 
## inversed matrix in memory for the argument x. If there is an inversed 
## matrix in memory, which was created ONLY with the previous call of this
## function for this particular argument X, then the function returns this
## value. If there is no object in memory, function creates this cached 
## value, calling the methods set_inv the object makeCacheMatrix.
##
## The changes in the original matrix are controled via the use of these 
## two functions. We can use cash, only calling cacheSolve. If we want to 
## change the matrix, we need to call makeChacheMatrix, which initializes 
## the cash for the matrix we want.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # getting the chache for the argument x, assuming that x was created
        # by makeCacheMatrix
        m <- x$get_inv()
        
        # checking, if the is a cached inversed matrix in memory for
        # the matrix x. If there is, return it. If no, processing further
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        # there was no cache. So, getting the original matrix
        data <- x$get()
        
        # not required by the task, but checking the dimensions of the 
        # original matrix, and if they are equal, calculation inversed 
        # matrix and putting it in into cache. Otherwise - error message
        
        if(length(data[1,]) == length(data[,1])) {
                m <- solve(data)
                x$set_inv(m)
        }
        else {message("matrix is not a square matrix"); return}
        
        # returning the inversed matrix, after putting it into cache
        m
}