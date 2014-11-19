## Below are two functions that are used to create a special object that stores a matrix
## and cache's its inverse

## The function "makeCacheMatrix" creates a special matrix which is a list containing a function to:
## set the value of the matrix, get the value of the matrix, set the value of the inverse and
## get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function (inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function "cacheSolve" calculates the inverse of the special matrix created with the above function. 
## Previously, it checks to see if the inverse has already been calculated in order to 1) to get the inverse 
## from the cache and skips the computation or 2) to calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) 
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
