## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL
set <- function(y) {
                x <<- y
                m <<- NULL
}

get <- function() x
     setInv <- function(inv) xinv <<- inv ## set the inversed matrix
     getInv <- function() xinv ## get the inversed matrix
     ## return a list that contains these functions, so that we can use
     ## makeCacheMatrix object like these
     ## x <- makeCacheMatrix(testmatrix)
     ## x$set(newmatrix) # to change matrix
     ## x$get # to get the setted matrix
     ## x$setInv # to set the inversed matrix
     ## x$getInv # to get the inversed matrix
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
 }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getInv() # get the inversed matrix from object x
     ## it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
     if(!is.null(m)) { ## if the inversion result is there
         message("getting cached data")
         return(m) ## return the calculated inversion
     }
     data <- x$get() ## if not, we do x$get to get the matrix object
     m <- solve(data) ## we solve it
     x$setInv(m) ## we then set it to the object
     m ## return the solved result
}
