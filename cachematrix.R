## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mi = matrix()) {  ##Matrix in
        mo <-NULL  ## Matrix Out
     
        set <- function(y=matrix()){
                mi <<- y  ## Matrix incoming
                mo  <<- NULL
        }
        
        get <- function() mi
        
        setmatrix <- function(y=matrix()) {
                ##library(MASS)
                mo <<- y
        }
        
        getmatrix <- function() mo
        
        list (set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(mi=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        mo <- mi$getmatrix()
        if (!is.null(mo)) {
                message("getting cached matrix")
                return(mo)
        }
        data <- mi$get()
        rc <- apply(y,c(1,2),solve)
        x$setmean(mo)
        mo
}
