## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function which will return a LIST containing the output of
## four functions:set,get,setsolve and getsolve. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

## set the matrix for the objects "x".
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

## "prints" the matrix for the objects "x".
        get <- function() x

## set the inverse matrix for the objects "x" in the global enviroment.

        setsolve <- function(inverse) inv <<- inverse

## prints the inverse matrix for the objects "x".
        getsolve <- function() inv

## return object of this makeCacheMatrix.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Function cacheSolve that allows us to calculate the inverse of the 
## of the matrix used in the previous makeCacheMatrix function. Note that
## the argument "x" passed to this below function is not a matrix but the object list
## obtained from makeCacheMatrix.

cacheSolve <- function(x, ...) {

## Define the inverse of the matrix in the list "x" and checks whether it's been already calculated. If it has been so,
## it returns that result as there is no need to calculate it again
        inv <- x$getsolve()  
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

## Inverse of the matrix in x has not been yet calculated so it is so now using the solve function. The inverse matrix result
## is now stored in the "x" so that if the same matrix is called again, there is no need to calcute its inverse again.

        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)

## Return of the inverse Matrix contained in "x".
        inv
}

