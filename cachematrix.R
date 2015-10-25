#RProgramming Assignment#2

##assigns function, where X is a matrix, 

makeCacheMatrix <- function(x = matrix()) {
##creates variable m  as null for caching
    m<-NULL
##establishes cache and a matrix in working environment   
    set<-function(y=matrix()){
        x<<-y
        m<<-NULL
  }
## assign get the passed matrix "x"  
  get<-function() x
  
##invert and store the matrix in cache "m"
  setinv<-function(solve) m<<- solve
##function to call the stored matrix  
  getinv<-function() m
  
  ##create list of functions to be called by cacheSolve
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


##cachesolve, checks to see if the inverse already exists, if so prints message and already existing data
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
##otherwise, no cached data, so calcualtes and prints inverse of passed matrix   
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}


