## These two functions cache the results of matrix inversion
## Caching saves the user time by not repeating expensive calculations


## The first function creates an object with "getter" and "setter" properties
## this is used to create objects that can be manipulated by the functions inside it
## it returns a list that contains the get/set functions and the matrix "x" and invMatrix

makeCacheMatrix <- function(x = matrix()) {
        #initiate m in this function's env
        m <- NULL
        
        #mutator aka "setter" functions
        set <- function(y) { 
                #use '<<-' to change x in its parent env, makeCacheMatrix
                x <<- y
                m <<- NULL
                #note, the only way to set the matrix also clears the inverse m
                #forcing the next function to recalculate the inv in order to cache it
                #this avoids the situation where the cache is not actually the correct inv
        }
        setInverse <- function(invMatrix) m <<- invMatrix
        
        #accesor aka "getter" functions
        get <- function() x #returns the matrix
        getInvMatrix <- function() m #returns the inverted matrix
        
        #the output of this function is a list, 
        #the contents of the list define the makeCacheMatrix objects
        #the list items are named to allow for the use of '$', e.g. x$get()
        list(set = set, get = get, setInverse = setInverse, getInvMatrix = getInvMatrix)
}

## The second function caches the inverse of the matrix
## if the matrix has changed it calculates the new inverse and caches it
## it can only use objects of type makeCacheMatrix as defined above

cacheSolve <- function(x, ...) {
        #note, we can use '<-' here because the object x has the same environment as makeCacheMatrix
        m <- x$getInvMatrix()
        #if the inverse exists already then return it
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        #otherwise if m is NULL, take the matrix and calculate the inverse
        mat <- x$get()
        #assigning the inv to "m", solve calculates inverse
        m <- solve(mat)
        #Cache the new inverse
        x$setInverse(m)
        m #returns m, the inverse
        
}
