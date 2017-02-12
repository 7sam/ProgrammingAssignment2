# Cousera R Programming Assign 2, Week III
# Peer graded 
# Lexical Scooping 
## This function calculates a matrix-object that can cache it's inverse. 


makeCacheMatrix <- function(x = matrix()) {
    ## assumptuion: x is always a square invertible matrix
    ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is used as the input to cacheSolve()
    
    m <- NULL
    set <- function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment (i.e. the outter loop). 
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated (assuming the matrix is idential), then the cacheSolve function will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 'x' is the output of our makeCacheMatrix() function
    ## We want to return: inverse of the original matrix input to makeCacheMatrix()
    m <- x$getinverse()
    # Where we use the $ to separate the data frame's name from the name of the variable
    
    # if the inverse has already been calculated above we do: 
    if(!is.null(m)) {
        # Where !is.null means that if our matrix inv is not (!) null 
        # get it from the cache and skips the computation 
        message("getting cached data")
        return(m)
    }
    # otherwise,we still need to calculate the inverse 
    data <- x$get()
    m <- solve(data, ...)
    # Note: "solve" is a generic function in the R package that solves the equation a %*% x = b for x, where b can be either a vector or a matrix
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinverse(m)
    m
}
