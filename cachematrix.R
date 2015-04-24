## This is an adaptation of the example provided which takes a matrix and creates and stores an inverse matrix.
## if the matrix supplied does not have a cached inverse version, one is created.
## tested case:  rbind(c(1, -1/4), c(-1/4, 1))
## expected:            [,1]      [,2]                  
##            [1,] 1.0666667 0.2666667  
##            [2,] 0.2666667 1.0666667 

## In order to better understand the example and the revised version, I added comments inline to help me understand what was happening.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                         # clears the stored value and sets x to the new matrix
                x <<- y                              # these use the '<<-' so they will be available outside this setting function
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve         # run inverse and saves to variable which is set as a value for outside functions
        getinv <- function() m                        # get the inv function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}    

## this function checks for an inverse matrix and returns it if it exists otherwise it gets the data from makeCacheMatrix and sets the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()                         # pulls getinv to see if it has a value
        if(!is.null(m)) {                       # if m is not null, pull value from cache
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         # if m is null, run get which pulls the value from makeCacheMatrix and assign the result to data
        m <- solve(data)                        # assign inverse of data to m
        x$setinv(m)                             # cache the value
        m                                       # return m which is the inverse of the original matrix
}