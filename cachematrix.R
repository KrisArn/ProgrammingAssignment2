## This is a cashe inverse matrix
makeCasheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function()x
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This is a cashe solve function - Return a matrix as inverse of x
casheSolve <- function(x, ...){
        inv <-x$getinv()
        if(!is.null(n)){
                message ("getting cashed data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
