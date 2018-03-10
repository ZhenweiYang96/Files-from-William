#Matrix inversion is usually a costly computation and there 
#may be some benefit to caching the inverse of a matrix rather 
#than compute it repeatedly (there are also alternatives to matrix 
#inversion that we will not discuss here). Your assignment is to 
#write a pair of functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.
#1. set the value of matrix
#2. get the value of matrix
#3. set the value of the inverse of the matrix
#4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
        inver <<- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) inver <<- inverse
        getinver <- function() inver
        list(set=set, get=get, setinver=setinver, getinver=getinver)
}

#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x,...){
        inver <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data)
        x$setinverse(inver)
        inver
}