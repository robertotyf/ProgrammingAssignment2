## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.

makeCacheMatrix <- function(x = matrix()) {
	# Following the same format as the assignment example
	# Creating a makeCacheMatrix object will consist of
	# four functions encapsulated in a list
	# 1. set the matrix
	# 2. get the matrix
	# 3. set the inverse of the matrix
	# 4. get the inverse of the matrix

	# Initially set to NULL
	# Changes when the user sets the value


	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
## cacheSolve function receives a variable that is a matrix that is expected to have been defined as makeCacheMatrix(),
## as in m <- makeCacheMatrix(), and then populated with an invertible matrix using the m$set() function that is nested 
## in makeCacheMatrix(). In this syntax, the variable "m" can be any letter. j See validation instruction, above.
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
