##Both the functions work together in order to pass a square
##matrix and obtain its inverse either from cache or by computing it.


## The first function,makeCacheMatrix returns a list of
##functions associated with the matrix whose inverse is to
##be evaluated or taken from cache by passing the square matrix
##as argument.


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  
  
  get<-function()x
  getinv<-function()inv
  setinv<-function(inverse)
  {
    inv<<-inverse  
  }
  list(set=set,get=get,getinv=getinv,setinv=setinv)

}


##The cacheSolve function accepts the matrix as argument
##and uses the makeCacheMatrix function's returned list
##of functions to access and set the cache inverse value
##as well as the matrix itself.


cacheSolve <- function(x, ...) {
  
  t<-x$getinv()
  if(!is.null(t))
  {message("Getting cached data")
    return(t)
  }
  data<-x$get()
  t<-solve(data, ...)
  x$setinv(t)
  return(t)
       
}
