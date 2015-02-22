#####################################################################################################
# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly. There is a pair of functions that cache the inverse of a matrix.
#####################################################################################################


#####################################################################################################
# 1. This function takes a matrix, saved in the private variable x and 
# creates a special "matrix" object that can cache its inverse
#####################################################################################################
makeCacheMatrix <- function(x = matrix()) {
        
        # initialize the matrix to NULL during the first call to makeCacheMatrix
        # this is needed because if getinverse() is called immediately after
        # the makeCacheMatrix funciton is constructed, without a call to setinverse
        # we know we must first calculate the matrix invesion in cacheSolve.
        inverse_m <- NULL
        
        
        # funciton to set a new value for the underlying matrix
        # this invalidates the cached inverse matrix, inversed_m;
        # the <<- operator is used to set the value of x and inverse_m because we want
        # to modify x and inverse_m defined in the enclosing environment (created
        # when makeCacheMatrix was first called), not in the environment local to set(),
        # in which x and inverse_m are undefined.
        # we must reset inverse_m to NULL since we are modifying the underlying
        # matrix and the cached value is no longer valid
        
        set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        
        # getter function for underlying matrix
        # in R the return value of a function is the last statement.
        get <- function() x
        
        # set the inverse of the matrix x.  Called by cachSolve,
        # the <<- operator is used because we want to modify the inverse_m defined
        # in the enclosing function makeCacheMatrix, not the inverse_m local to setinverse,
        # which would be undefined.
        setinverse <- function(inverse) inverse_m <<- inverse
        
        # returns the inverse matrix.  Will be null if setinverse has not been called or
        # if set is called after the last call to setinverse
        getinverse <- function() inverse_m
        
        # return value of the makeCacheMatrix function is a list
        # of functions (and variables if we wish) that we want to expose
        # as public.  these are accessed with the $ operator.  Any variables
        # declared inside makeCacheMatrix but not exported as part of this list
        # are private...they are inaccessible to any caller of makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

############################################################################################################
# 2. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache. cacheSolve takes a caching matrix created with makeCacheMatrix
#############################################################################################################
cacheSolve <- function(x, ...) {
        
        # get the inverse of the matrix defined inside x.
        # we can use the $ operator to access the function since it was
        # defined in the list of function pointers returned by the call to
        # makeCacheMatrix
        inverse_m <- x$getinverse()
        
        # if we've already computed the inverse matrix and stored it via setinverse(),
        # and have not invalidated the cache by calling set(), return the cached
        # version of x
        if(!is.null(inverse_m)) {
                message("getting cached data")
                
                # we have to explicily use return here otherwise we'd keep
                # executing the code after the if conditional ends.  Since
                # the cached version is good, just return it and we are done.
                return(inverse_m)
        }
        
        # either we haven't computed the cached version yet, or we've called
        # set() previously and invalidated the cache.
        
        # call get() to get the underlying inverse matrix
        data <- x$get()
        
        # calculate the inverse of the underlying matrix, passing with it
        # any var args passed to cacheSolve
        inverse_m <- solve(data, ...)
        
        # now set the inverse matrix in x so we cache it and dont need to needlessly
        # recompute it
        x$setinverse(inverse_m)
        
        
        # return the caching matrix
        inverse_m
}

# TEST: generate matrix, and the inverse of the matrix.
size <- 1000 # size of the matrix edge
test_matrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
test_matrix_inverse <- solve(test_matrix)

# solve the matrix via the cache method
special_matrix   <- makeCacheMatrix(test_matrix)

# this should take longer than the next call since there will be 
# an inverse computation performed for the first time
special_matrix_inverse1 <- cacheSolve(special_matrix)

# this should retrieve cached matrix calculated in the first call,
# 'getting cached data' should get displayed to confirm the logic
special_matrix_inverse2 <- cacheSolve(special_matrix)

# this returns TRUE to confirm that all solved matrices are identical
identical(test_matrix_inverse, special_matrix_inverse1) & identical(test_matrix_inverse, special_matrix_inverse2)


