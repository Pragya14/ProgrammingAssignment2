## The idea is to cache the computation of inverse so that there is no need to compute it everytime we need it.
##Hence we are creating a set of functions which enable us to store the data and its inverse and use it in another function.

## makeCachematrix: This is a vector which stores multiple functions and input values for cache and returns them when needed

makeCacheMatrix <- function(x = matrix()) {
   set<-function(x){
       y<<-x
       i<<-NULL
   } #First function to set the input matrix
   get<-function()x #second function to return the values as per need
   setinverse<-function(inverse){
       i<<-inverse
   }# third function to store the inverse through user input
   getinverse<-function()i#4th function to return the value on demand
   list(
       set,get,setinverse,getinverse)#storing the list of functions
}
## Cachesolve: This function calculates the inverse of the matrix after taking inputs from makeCachematrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   i<-makeCacheMatrix$getinverse()
    if(!is.null(i)){
        return(i)
    } #using the cache memory to retrive the inverse
   data<-makeCachematrix$get()
   i<-solve(data,..)
   makeCacheMatrix$setinverse(i)
   i
   # if not available directly, compute it through function solve
}
