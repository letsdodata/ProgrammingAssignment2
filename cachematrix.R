## The file creates a special kind of matirx
## which stores the inverse of the matrix in the cache,
## thus it aves time on the extra computations of inversed matrix

## The following function function creates a "set up" of the matrix
## with addtional field for cached data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_Inverse <- function(inverse) inv <<- inverse
        get_Inverse <- function() inv
        list(set = set, get = get,
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)
}

## The following function retrieves the cached inverse if it was computed and matrix 
## has not been changed since then.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_Inverse()
        
        #if inverse was already computed we take from the cache
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #otherwise compute the inverse
        
        data <- x$get()
        inv <- solve(data, ...)
        x$set_Inverse(inv)
        inv
}