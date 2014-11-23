##      This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##      initially nothing is cached so set it 'm' to NULL
        m <- NULL
        ##      set the value of the Matrix
        set<-function(y){
                x <<- y
                m <<- NULL
        }
        ##      get the value of the Matrix
        get <- function() x
        ##      set the value of the Inverse of matrix
        setmatrix <- function(solve) m<<- solve
        ##      get the value of the Inverse of matrix  
        getmatrix <- function() m
        ##      return a list of names which are each functions
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
                
}


##      The following function calculates the inverse of a "special" matrix created with 
##      the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ##      get matrix 'm' from Cache
        m <- x$getmatrix()
        #       if a cached value exists in 'm' then return it
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        #       if a cached value 'm' is null then get referenced matrix
        matrix <- x$get()
        ##      calculate value of the inverse of matrix
        m <- solve(matrix, ...)
        ##      set the inverse of the matrix 
        x$setmatrix(m)
        ##      return a matrix that is the inverse of 'x'
        m
}

