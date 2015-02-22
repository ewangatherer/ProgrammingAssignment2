## makeCacheMatrix provides 4 functions, set, get setmatrix and getmatrix
## providing the capability to create & cache a matrix
## cacheSolve inverts a matrix, leveraging the potentially cached matrix
## cacheSolve will store the inverted matrix for later retrieval on the first execution

makeCacheMatrix <- function(x = matrix()) {
    
    #2 variables are maintained, x as the new matrix, mat as the cached matrix
    
    mat <- NULL
    #set takes the input, applies it to the orginating matrix and overwrites any cached matrix
    set <- function(y) {
        x <<- y
        mat <- NULL
    }
    #simple return of the existing matrix by means of a function
    get <- function() x
    
    #sets the cached value to that of the variable inverse by means of a function
    setmatrix <- function(inverse) mat <<- inverse
    
    #returns a function that returns the value of the cached matrix
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

