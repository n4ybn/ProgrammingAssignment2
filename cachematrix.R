## DLD: July 26, 2015 
## This program creates a Matrix with four functions.
## GET,SET,GETMATRIX, SETMATRIX
## These function similarly to the Vector example in the assignment.
## 1. Set up an input Matrix like this:
## 2.   a <- matrix(31:66,6,6)
## 3. Execute function like this: source(D:\your_folder\cachematrix.R)
## 4. mtx <- cacheSolve(a)
## 5. mtx$get() - returns the input matrix
## 6. mtx$getmatrix() - returns the inverse of the matrix


## This Function: makeCacheMatrix 
## accepts a matrix and returns a matrix with four sub functions

makeCacheMatrix <- function(mi = matrix()) { ##Matrix in is 'mi'
        mo <-NULL  ## Matrix Out
        
        set <- function(y=matrix()){
                mi <<- y  ## Matrix incoming
                mo  <<- NULL
        }
        
        get <- function() {
                if (is.matrix(mi)) {
                        return(mi)
                }
                return (NULL)
        }
        setmatrix <- function(y=matrix()) {
                ##library(MASS)
                mo <<- apply(y,c(1,2),solve)
        }
        
        getmatrix <- function() mo
        
        list (set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}


## This function cacheSolve:
## calls the MakeCacheMatrix function to build a matrix 
## and uses the matrix's functions to check to see if the inverse is cached.
## This function also returns a message to the screen regarding the cached result.

cacheSolve <- function(mi=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        mtx <- makeCacheMatrix(mi)
        mo <- mtx$getmatrix()
        if (!is.null(mo)) {
                message("getting cached matrix")
                return(mo)
        }
        else { message("Not Cached, computing")
                data <- mtx$get()
                if (is.matrix(data)) {
                        mo <- apply(data,c(1,2),solve)
                        mtx$setmatrix(mi)
                }
        }
        mtx
}
