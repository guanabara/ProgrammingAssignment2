## makeCacheMatrix is a function wich defines an object type used to store a matrix, and to cache its inverse. 
## This type only has setters and getters to both values.
## It makes uses of R scoping in order to preserve the inverse value inside and R object. 


makeCacheMatrix <- function(x = matrix()) {
    mtxInverse <- NULL
        
    set <- function(mtx)
    {
        x <<- mtx
        mtxInverse <<- NULL
    }
    
    get <- function() { x }
    
    setInverse <- function(inverse) { mtxInverse <<- inverse}
    
    getInverse <- function() { mtxInverse }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse
         )
}


## The following function calculates the inverse of a given matrix created by makeCacheMatrix, and returns the cached value if it exists,
## otherwise calculate it using R solve function, and store in cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    if (!is.null(m))
    {
        message("getting cached data.")
        return(m)
    }
    message("calculating inverse for matrix and populating cache.")
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}

