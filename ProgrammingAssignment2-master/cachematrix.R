
## This is a function to create a special matrix
## that can be inversed and cached
makeCacheMatrix <- function(x = matrix()){  ## Defines the function, with default mode of Matrix    
        i <- NULL                           ## initializes i as NULL    
        set <- function(y){                 ## Defines a function set
                x <<- y                     
                i <<- NULL                  ## Resets i to NULL if new matrix exists    
        }
        get <- function()                   ## Defines the get function
                x                           ## returns the value of the matrix
                
        setinverse <- function(inverse)     ## Defines the setinverse funtion
                i <<- inverse               ## assigns i value in the parent environment
        getinverse <- function()            ## Defines getinverse as a function
                i                           ## Returns the value of i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This is a function to solve and cache the special
## matrix created above
cacheSolve <- function (x, ...){            ## Defines the function
        i <- x$getinverse()                 ## Defines i 
        if(!is.null(i)){                    ## Checks whether i already exists
                message("Fetching Cached Matrix")
                return(i)                   ## Returns the stored i
        }
        mat <- x$get()                      ## Defines mat
        i <- solve(mat, ...)                ## Defines i as the inverse of mat
        x$setinverse(i)                     
        i                                   ## Returns i
}


