#http://users.utu.fi/attenka/julia_set.R
a <- -0.7;b <- -0.4 # Complex parameter, connected to coordinate of the Mandelbrot set in a complex plane. Constants here.
lims <- c(-2,2)
maxIter <- 60
cl <- colours()
step <- seq(lims[1],lims[2],len=1000)
M <- matrix(0,length(step)^2,3)
a1 <- 0
l <- length(step)

for(x in step) {
	for(y in step) {
		n <- 0
		DIST <- 0
		x1 <- x; y1 <- y # Original x and y are saved.
		while(n<maxIter & DIST<4) {
			newx <- x1^2-y1^2+a
			newy <- 2*x1*y1+b
			DIST <- newx^2+newy^2
			x1 <- newx;y1 <- newy
			n <- n+1
		}
		if(DIST<4) colour <- 24 else colour <- n*10
		a1 <- a1+1
    cat(paste0("\r",round(100*a1/l^2),"%"))
		M[a1,] <- c(x,y,colour)
	}
}

plot(M[,1], M[,2], xlim=lims, ylim=lims, col=cl[M[,3]], pch=".")
