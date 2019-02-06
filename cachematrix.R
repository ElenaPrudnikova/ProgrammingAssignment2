## Functions will cache the inverse of the matrix


## Function creates a special "matrix" object that can cach its inverse

makeCacheMatrix <- function(x = matrix()) {
              s<-NULL
              set<-function (y) {
                x<<-y
                s<<-NULL
              }
              get<-function() x
              setinverse<-function(solve) s<<-solve
              getinverse<-function() s
              list(set=set, get=get,
                   setinverse=setinverse,
                   getinverse=getinverse)
}


## Function computes the inverse of special "matrix" returned by makeCacheMatrix. If the inverse has already been
## calculated ( and the matrix has not changed) then the inverse matrix from the cach will be retrieved

cacheSolve <- function(x, ...) {
        s<-x$getinverse()
        if(!is.null(s)){
          message("getting cached data")
          return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setinverse(s)
        s
}
