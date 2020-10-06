## Put comments here that give an overall description of what your
## functions do

## function to create the matrix with its cache

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInv<-function(inversa) inv <<- inversa
  getInv<-function() inv
  list(set = set,get = get,setInv=setInv,getInv=getInv)
}

## funcion para generar la inversa y guardarla en cache o recuperarla

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("fetching the cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
}
