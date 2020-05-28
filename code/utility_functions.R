# Utility functions

# conversion functions
f2d <- function(f,ng){
  sqrt((f**2)*2*ng)
}

rs2f <- function(r2){
  sqrt(r2/(1-r2))
}

r2d <- function(r){
  (2*sqrt(r))/(sqrt(1-r))
}