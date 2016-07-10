## following two functions are used to cache the inverse of a matrix
## and return it from cache instead of calculating again if the matrix is not changed

## Write a short comment describing this function
## a) set the value of the matrix 
## b) get the value of the matrix
## c) set the value of inverse of the matrix
## d) get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

	invr <- NULL 
        set <- function(y) { 
        x <<- y 
        invr <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inverse) invr <<- inverse 
        getinverse <- function() invr 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 

}


## This function returns the inverse of the matrix. 
## a) Check if the inverse has already been computed. 
## b) If yes, get the result and skip the calculation.
## c) If not, compute the inverse, sets the value in the cache

cacheSolve <- function(x, ...) {
        invr <- x$getinverse() 
        if(!is.null(invr)) { 
           message("getting data from cache.") 
           return(invr) 
        } 
        data <- x$get() 
        invr <- solve(data) 
        x$setinverse(invr) 
        invr
}


## Output from executing the program in R Studio
## > x <- rbind(c(1,2),c(3,4))
## > i <- makeCacheMatrix(x)
## > i$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
##
## Calculates the inverse the first time
## > cacheSolve(i)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## Gets from Cache the 2nd time as the matrix has not been changed
## > cacheSolve(i)
## getting data from cache.
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## Assign a new set of values to the matrix using set function
## > i$set(rbind(c(5,6),c(7,8)))
## > i$get()
##      [,1] [,2]
## [1,]    5    6
## [2,]    7    8
##
## Calculates the inverse again as the matrix has been changed
## > cacheSolve(i)
##      [,1] [,2]
## [1,] -4.0  3.0
## [2,]  3.5 -2.5
##
## Gets from Cache as the matrix has not been changed
## > cacheSolve(i)
## getting data from cache.
##      [,1] [,2]
## [1,] -4.0  3.0
## [2,]  3.5 -2.5
##> 
