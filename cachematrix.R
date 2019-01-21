makeCacheMatrixi <- function(x = matrix()) {
        inv_mat <- NULL
        set_mat <- function(mat) {
                x <<- mat
                inv_mat <<- NULL
        }
        get_mat <- function() x
        setInverse_mat <- function(inverse_mat) inv_mat <<- inverse_mat
        getInverse_mat <- function() inv_mat
        list(set = set_mat,
             get = get_mat,
             setInverse = setInverse_mat,
             getInverse = getInverse_mat)
}


## Write a short comment describing this function

cacheSolvei <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getInverse()
        if (!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        fmat <- x$get()
        inv_mat <- solve(fmat, ...)
        x$setInverse(inv_mat)
        inv_mat
}