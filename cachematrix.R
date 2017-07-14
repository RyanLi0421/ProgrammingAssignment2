## Week3 Peer-Graded Assignment: Caching the Inverse of a Matrix
## Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # define a cache to store inversed matrix
        invs <- NULL
        
        #define nested set x function  
        set_x <- function(y) {
                x <<- y
                invs <<- NULL
        }
        
        # define nested get x function
        get_x <- function(){ x } 
        
        # define nested set cache function
        set_invs <- function(inverse){
                invs <<- inverse
        }
        
        # define nested get cache function
        get_invs <- function(){ invs } 
        
        # return a function list
        list(set_x = set_x, 
             get_x = get_x,
             set_invs = set_invs,
             get_invs = get_invs)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(cache, ...) {
        ## Return a matrix that is the inverse of 'x'
        # see if inverse already exist, if exist message and retrieve
        invs <- cache$get_invs()
        if(!is.null(invs)){
                message("getting cached data")
                return(invs)
        }
        
        # or else, calculate, store and return
        data <- cache$get_x()
        invs <- solve(data, ...)
        cache$set_invs(invs)
        invs
}
