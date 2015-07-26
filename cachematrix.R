
## R Programming - Coursera : Programming Assignment 2
## kpcorda

## Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
# of a matrix rather than computing it repeatedly.

# This function creates a special "matrix" object that can cache its inverse,
#  which contains a function to
# i.    set the value of the matrix
# ii.   get the value of the matrix
# iii.  set the value of the inverse of the matrix
# iv.   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
     
        inverseMatrix <- NULL
        # set the value of the matrix
        setMatrix <- function(y)     
        {
                x <<- y
                inverseMatrix <<- NULL
        }
        # get the value of the matrix
        getMatrix <- function() x
        # set the value of the inverse of the matrix
        setInverseMatrix <- function(solve) inverseMatrix <<- solve
        # get the value of the inverse of the matrix
        getInverseMatrix <- function() inverseMatrix
        # returns list
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)   
             
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve retrieves the inverse from the cache.

# Assumption: The matrix supplied is always a square invertible matrix.

cacheSolve <- function(x, ...) 
{
     # Return a matrix that is the inverse of 'x'
     
      inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("Getting cached data!")
                return(inverseMatrix)
        }
        matrix <- x$getMatrix()
        # The inverse of a square matrix can be done with the solve function in R.
        inverseMatrix <- solve(matrix)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}

