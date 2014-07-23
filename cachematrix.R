## PROBLEM: Build a data structure needed to further perform the function of 
##          "inverse matrix".
## CONTRACT: Square matrix that can be inverted.
## EXAMPLES: INPUT: matrix(rnorm(16),4,4); matrix(rnorm(9),3,3); matrix(1:4,2,2)
## TEST:
##
## > makeCacheMatrix(M)
## $set
## function (y) 
## {
##        x <<- y
##        s <<- NULL
## }
## <environment: 0x7fd6db898a98>
##        
##        $get
## function () 
##        x
## <environment: 0x7fd6db898a98>
##        
##        $setmean
## function (solve) 
##        s <<- solve
## <environment: 0x7fd6db898a98>
##        
##        $getmean
## function () 
##        m
## <environment: 0x7fd6db898a98>
        
makeCacheMatrix <- function(x = matrix()) {
        s       <- NULL
        set     <- function(y) {
                x <<- y
                s <<- NULL
        }
        get     <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}
## PROBLEM: Function that uses a special data structure to calculate 
##          the "inverse matrix". Consider that if the data structure 
##          does not change, to be invoked only gives the above result.
## CONTRACT: Data structure built by the function: makeCacheMatri
## TEST:
##
## > M <- matrix(rnorm(9),3,3)
## > cm <- makeCacheMatrix(M)
## > cacheSolve(cm)
## [,1]       [,2]        [,3]
## [1,] -0.59761422 0.04363483 -0.08464989
## [2,]  0.04015043 0.07536724  0.77730413
## [3,] -0.67250582 0.82590896 -0.68980481
## 
## > cacheSolve(cm)
## getting cached data
## [,1]       [,2]        [,3]
## [1,] -0.59761422 0.04363483 -0.08464989
## [2,]  0.04015043 0.07536724  0.77730413
## [3,] -0.67250582 0.82590896 -0.68980481

cacheSolve <- function(x, ...) {
        s       <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data    <- x$get()
        s       <- solve(data, ...)
        x$setsolve(s)
        s
}
