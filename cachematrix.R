## Solve the inverse of a matrix, using internal cache
## Example of use:
## > x<-makeCacheMatrix(matrix(4:1,2,2))
## > y<-cacheSolve(x)
## > y
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
## > y<-cacheSolve(x)
## getting cached data

##  create a wrapper object of Matrix, and 
##  store its inverse cache
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # inverse of x
    set <- function(y) {
        x <<- y
        inv <<- NULL
      }
    get <- function() x
    setinv <- function(solvedInv) inv <<-solvedInv
    getinv <- function() inv
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## calculate the inverse of a matrix created by makeCacheMatrix
## , using its internal cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    return(x$setinv(solve(x$get())))
}

