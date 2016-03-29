## Hi This is Max Solanki editing this R file for assignment # 2


## Here I will write the following functions
## 1. makeCacheMatrix - This function will creates a special
## matrix object that can cache its inverse
## 2. cacheSolve - This function will compute the inverse of the
## special matrix above.  If matrix has already been created and 
## not changed then this function will retrieve the inverse from
## the cache


## Here is the makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {##define w/default
 inv <- NULL                               ## Initialize
 set <- function(y) {                      ## define set func
		x <<- y                         ## value in parent env   
		inv <<- NULL                    ## if new, set NULL
	}
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() inv

 list(set = set,	 get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")	
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

