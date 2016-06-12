##These are two functions to demonstrate caching variables while finding an
##inverse of a matrix. Finding an inverse of a matrix can be time consuming and 
##the practice of using the <<- operator to cache variables as the calculations 
##progress can make these types of operations more efficient.

##Function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        ## accepts input to create a square invertible matrix
        ## return: a list containing functions to
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        ## this list is used as the input to InvCacheMatrix()
        
        inv <- NULL
        set <- function(m) {
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        


        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##cacheSolve function
cacheSolve <- function(x, ...) {
        ## accept output of makeCacheMatrix()
        ## return: inverse of the matrix input to makeCacheMatrix()
        
        inv <- x$getinv()
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        matdata <- x$get()
        inv <- solve(matdata, ...)
        x$setinv(inv)
        return(inv)
}