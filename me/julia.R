library(MASS)
a <- -.25+0i

f <- function(x,c=a) x^2+c # |c| = sqrt(a^2+b^2) < 2

Fx <- function(x,it=0,c=a) {
  if (it==20) {
    f(x)
  } else {  
    Fx(f(x),it+1)
  }
}

n <- 1000
#x <- seq(-.2,.2,len=n)
#y <- seq(-.2,.2,len=n)
x <- rnorm(n)
y <- rnorm(n)
M <- matrix(0,n,n) 

for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    M[i,j] <- complex(re=x[i],im=y[j])
  }
}


Z <- Fx(M)
ok <- Z[which(!(is.na(Z)) & abs(Z)<2)]
plot(Re(ok),Im(ok),pch=20)
