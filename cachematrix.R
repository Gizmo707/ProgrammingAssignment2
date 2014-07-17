## This functions exploit the enviroment nesting and lexical scopping
## of the R system to chache a matrix and its inverse in order to avoid
## repeated computation.
##
## Note: The matrix is assumed to be invertible
## 
## To use:
##   1- Create an cache object (a list) with makeCacheMatrix.
##      Either associate the matrix in the function call or use set:
##
##      > mat <- matrix(c(1,2,4,3),2,2) # a 2x2 matrix
##
##      > c_mat <- makeCacheMatrix(mat)
##
##      or
##
##      > c_mat = makeCacheMatrix()
##      > c_mat$set(mat)
##
##   2- Compute and store the inverse with cacheSolve,
##      passing as parameter the object created by makeCachedMatrix
##      The first time this cacheSolve is called it will compute the
##      inverse; subsequent calls will return the cached inverse:
##
##      > cacheSolve(c_mat)
##           [,1] [,2]
##      [1,] -0.6  0.8
##      [2,]  0.4 -0.2
##      > cacheSolve(c_mat)
##      Gettiting cached data
##           [,1] [,2]
##      [1,] -0.6  0.8
##      [2,]  0.4 -0.2
##
##   3- To change the matrix use set:
##
##      > mat2 <- matrix(c(1,2,3,6,5,4,8,7,9),3,3)
##      > c_mat$set(mat2)
##      > cacheSolve(c_mat)
##                 [,1]       [,2]       [,3]
##      [1,] -0.8095238  1.0476190 -0.0952381
##      [2,] -0.1428571  0.7142857 -0.4285714
##      [3,]  0.3333333 -0.6666667  0.3333333
##      > cacheSolve(c_mat)
##      Gettiting cached data
##                 [,1]       [,2]       [,3]
##      [1,] -0.8095238  1.0476190 -0.0952381
##      [2,] -0.1428571  0.7142857 -0.4285714
##      [3,]  0.3333333 -0.6666667  0.3333333


## makeCacheMatix returns an list of four functions used to
## store a matrix and its inverse in its enviroment:
##
##   1- set(m) stores m as a new matrix and invalidates its inverse
##   2- get() returns the stored matrix
##   3- setinverse(im) stores im as the inverse matrix
##   4- getinverse() returs the stored inverse matrix or NULL if not present
##
## Argument (optional) a matrix to store in its enviroment

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes, stores and returns the inverse of a matrix
##
## If the inverse has already been computed, a cached copy is returned,
## otherwise, the inverse is computed and stored
##
## Argument: a list of functions created by makeCacheMatrix.
##           The matrix to invert must already have been stored
##           in the object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Gettiting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}
