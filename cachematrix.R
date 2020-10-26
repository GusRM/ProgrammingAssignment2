## These two functions are create to calculate the inverse of a matrix


## This function creates a special matrix than can cache its inverse

makeCacheMatrix<- function(x = matrix()) {
          m<-NULL
          set<-function(y){
                x<<-y
                m<<-NULL
          }  
          get<-function() x
          setinverse<-function(solve) m<<-solve
          getinverse<-function() m
          list(set=set,get=get,
               setinverse=setinverse
               ,getinverse=getinverse)

          }



## This function computes the inverse of the special matrix from above

cachesolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m
}
