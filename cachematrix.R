### Matrix inversion is usually a costly computation 
### and there may be some benefit to caching the inverse of a matrix rather than compute 
### it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
### Your assignment is to write a pair of functions that cache the inverse of a matrix.

### This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
### 1. set the value of the matrix
        set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
        }
### 2. get the value of the matrix
        get <- function() x

### 3. set the value of the inverse of the matrix
        set_inverse <- function(inverse) inv_matrix <<- inverse

### 4. get the value of the inverse of the matrix
        get_inverse <- function() inv_matrix

### 5. return the list
        list(set = set, get = get, set_inverse = set_inverse,get_inverse = get_inverse)
        }

### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return the matrix that is the inverse of the matrix
### 1. get inverse of the matrix
        inv_matrix <- x$get_inverse()
### 2. if inverse of the matrix exists, check if already cached
### if yes, return cached inverse
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
### 3. if not, get the matrix 
        data <- x$get()
### 4. compute inverse of the matrix
        inv_matrix <- solve(data)
### 5. cache inverse of the matrix
        x$set_inverse(inv_matrix)
### 6. return inverse of the matrix
        inv_matrix
}
