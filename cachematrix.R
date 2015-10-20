## Put comments here that give an overall description of what your
## functions do

## ############################################################
# makeCacheMatrix : 
#   takes a matrix as argument and stores it, caches the inverse 
#   so as to avoid re-inverting the matrix
# provides following functions to manipulate the matrix :
# get        : returns the matrix
# set        : stores/replaces the matrix
# getInverse : returns the cached inverse of matrix
# setInverse : sets (caches) the inverse of the matrix. SHOULD ONLY BE
#              called by the companion function cachesolve (see below)
## ############################################################


makeCacheMatrix <- function (m = matrix()) {
    inv <- NULL
    set <- function(mnew) {
        m <<- mnew
        inv <- NULL
    }
    get <- function() m
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## ############################################################
# cachesolve : companion function to makeCacheMatrix, returns inverse of a matrix
# created using makeCacheMatrix. It calculates the inverse teh first time and returns 
# cached inverse on subsequent calls
# and uses 
## ############################################################
cachesolve <- function (m, ...) {

    inv <- m$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        inv
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setInverse(inv)
    inv
}

