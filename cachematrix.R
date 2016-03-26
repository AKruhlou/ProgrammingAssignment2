## Finding the inverse matrix
## work with the cache

## Return the list of functions for cache: set(x), get(), set_inv(x), get_inv()

makeCacheMatrix <- function(m = matrix()) {
        m_inv <- NULL
        
        # cached the value of the matrix and clear its inverse value 
        set <- function(x) {
                m <<- x
                m_inv <<- NULL
        }
        # get the value of the matrix "m" from cache 
        get <- function() m
        
        # cached the value of the inverse matrix
        set_inverse <- function(x) m_inv <<- x
        # get the value of the inverse matrix "m_inv" from cache
        get_inverse <- function() m_inv
    
        list(set = set, get = get, set_inv = set_inverse, get_inv = get_inverse)
}


## Return a matrix that is the inverse of 'm'

cacheSolve <- function(m, ...) {
        
        # try to retrieve the inverse matrix from the cache
        m_inv <- m$get_inv()
        
        if(!is.null(m_inv)) {
            message("getting cached inverse matrix")
            return(m_inv)
        }
        # find the inverse matrix and cached it
        m_inv <- solve(m$get(), ...)
        m$set_inv(m_inv)
        
        # control: retrieve the inverse value from the cache
        m$get_inv()
}
