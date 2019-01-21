#The The first function,makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
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

#For example by running the following code you will get this:
# test <- makeCacheMatrix()
# test$set(matrix(2:5, 2))
# test$get()
#      [,1] [,2]
#[1,]    2    4
#[2,]    3    5
## The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
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

#In keeping with the previous example by running the following code you will get:
#cacheSolve(test)
#    [,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1