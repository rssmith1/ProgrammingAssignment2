## These functions implement the equivalent of the "mean" example except it changes the data type as the subject from a numeric vector
## to a matrix.

#The first function, makeCacheMatrix creates a special "vector", which is really a 
#list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the matrix inverse 
#get the value of the matrix inverse

#This function only needs to be called once.  
#Subsequent use of this datastructure is through the elements of the list.  For example
#create the special vector:
#sv<-makeCacheVector(matrix(1:4, ncol=2, nrow=2))
# then
# get the vector
# sv$get()
# set a new vector 
# sv$set(matrix(1:9,ncol=3,nrow=3))

# note you could call sv$getsolve directly, but that defeats the purpose.
# instead call cacheSolve(sv) which will return the inverse, but only calculate it if it has not been calculated 

## NB m and x are referenced in the parent frame of these functions.  Overwriting them 
## between function calls to cacheMean would be like crossing the streams.
## m represents the inverse of the matrix obtained by solve. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
           setsolve = setsolve,
          getsolve = getsolve)
}


## Takes as input the "special vector"that is created by makeCacheMatrix. 
## it will return the inverse UNLESS it has not yet been calculated OR the data was "set" and the
## inverse has not yet been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    print(data)
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

