## makeCacheMatrix provides 4 functions, set, get setmatrix and getmatrix
## providing the capability to cache a matrix
## cacheSolve inverts a matrix, leveraging the potentially cached matrix
## cacheSolve will store the inverted matrix for later retrieval on the first execution

makeCacheMatrix <- function(x = matrix()) {

    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <- NULL
    }
    
    get <- function() x
    
    setmatrix <- function(inverse) mat <<- inverse
    
    getmatrix <- function() mat
    list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
    
}


## cacheSolve checks if the matrix already exists, if so returns the existing matrix
## if not it leverages solve to invert the matrix, stores the inverted matrix and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getmatrix()
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setmatrix(mat)
    mat
}
