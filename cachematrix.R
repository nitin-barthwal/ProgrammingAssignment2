## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



## This method creates a special vector 'makeCacheMatrix' 
# This vector describes 4 methods namely - 
#set,get,setsolve snd getsolve.



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL    # Inverse matrix set to NULL
        set <- function(y) {
                x <<- y     # Set a new Matrix 
                m <<- NULL  # Reset the Inverse

        }
        get <- function() x     # Return the Matrix
        setsolve <- function(solve) m <<- solve  # store the inverse of matrix in m
        getsolve <- function() m  # get the stored inverse matrix
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This method checks where there exit inverse of the matrix in cache , if yes then return value from cache 
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

# Checking the methods

mvec <- makeCacheMatrix() 
x3 <- matrix(1:4,2,2)  # defining a  matrix
mvec$set(x3)   # store the matrix
mvec$get()  # display the matrix


mvec$getsolve()  # returns NULL
mvec$setsolve(solve(x3))    # Calculates the inverse of the matrix
mvec$getsolve()   # set the inverse in cache

cacheSolve(mvec)  # instead of calculating, return cached inverse.