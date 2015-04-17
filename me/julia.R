source("countdown.R")
f <- function(x,c=-.7-.4i) x^2+c # |c| = sqrt(a^2+b^2) < 2
#f <- function(x,c=.3-.5i) x^2+c # |c| = sqrt(a^2+b^2) < 2

Fx <- function(x,it=0,maxIts=60,col=1,bound=2) {
  if (it==maxIts) {
    24 # black
  } else {
    ifelse(abs(x)>bound,col*10, Fx(f(x),it+1,col=col+1))
  }
}

n <- 1000
x <- seq(-2,2,len=n)
y <- seq(-2,2,len=n)
lx <- length(x)
ly <- length(y)
M <- matrix(0,n,n) 

# Loops
for (i in 1:lx){
  ot <- Sys.time()
  for (j in 1:ly) {
    M[i,j] <- complex(re=x[i],im=y[j])
  }
  count.down(ot,i,lx)
}

Z2 <- Fx(M,maxIts=100)
#svg("manderboltIndian.svg",height=13,width=23)
par("bg"="black"); image(Z2,col=paste0("grey",1:100),useRaster=T)
