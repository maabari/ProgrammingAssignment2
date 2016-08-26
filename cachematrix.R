#the following functions are used to cache the inverse of a matrix

#four functions allow to modify (set) or (get) the value of the
#inverted matrix and to recalculate (set) and get the value of its
#inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        #setting the mean to NULL as a placeholder for future value
        m <- NULL
        
        #define a function to vector x to a new vector y and reset the mean m to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #return x
        get <- function() x
        
        #set and return m
        setinv <- function(solve) m <<- solve
        getinv<- function() m
        
        #returns the list containing above functions defined
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#the following function returns the inverse of the matrix that was
#created with the previous function (makeCacheMatrix). If the inverse
#has not been calculated (is.null), or the matrix changed recently,
#then a recalculation would be executed.


cacheSolve <- function(x, ...)
{
        m <- x$getinv()
        if (is.null(m)) {
                message('Computing inverse...')
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
        } else {
                message('Returning cached inverse...')
        }
        return(m)
}

#
# Testing
#

# Create a cached matrix
x <- makeCacheMatrix()
set.seed(123)
x$set(matrix(runif(16, -1, 1), 4))

# cacheSolve() computes the inverse
cacheSolve(x)

# cacheSolve() returns the cached inverse
cacheSolve(x)

# After changing the matrix, cacheSolve() recomputes the inverse
x$set(matrix(runif(16, -1, 1), 4))
cacheSolve(x)
