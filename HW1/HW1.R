x <- seq(-4, 7, length=100)

y1 <- dnorm(x, mean = 1,sd = 2)
y2 <- dnorm(x, mean = 2,sd = 2)
y <- y1*.5 + y2*.5

plot(x,y1,type="l")
lines(x,y2)
lines(x,y, col= "red", lwd = 4)

######

y1 <- dnorm(x, mean = 1,sd = .2)
y2 <- dnorm(x, mean = 2,sd = .2)
y <- y1*.5 + y2*.5
plot(x,y, type = "l" , lwd = 3, col = 1)
s <- c(.3,.4,.6,1,2,5)

for (i in 1:length(s)){
  sd=s[i]
  y1 <- dnorm(x, mean = 1,sd)
  y2 <- dnorm(x, mean = 2,sd)
  y <- y1*.5 + y2*.5
  lines(x,y, col=i+1, lwd=3)
}



############################################################
############################################################

require(plot3D)

ps = .51
pl = .49

pSAs = .93 ; pSBs = .87 ; pSAl = .73 ; pSBl = .69

pAs <- .24 ;  pAl <- .77
(ps*pAs*pSAs + pl*pAl*pSAl)
(ps*(1-pAs)*pSBs + pl*(1-pAl)*pSBl)

remove(pAs, pAl)
pAs <- x <- seq(0,1,length.out = 100)
pAl <- y <- seq(0,1,length.out = 100)

(M <- mesh(x,y))

rho <- function(x,y){
  rho = ((ps*x*pSAs + pl*y*pSAl)/(ps*(1-x)*pSBs + pl*(1-y)*pSBl)) * ((1-x)*ps + (1-y)*pl)/(x*ps + y*pl)
}

V <- with (M, rho(x,y))

contour2D(z = V, nlevels=1, level =0, lwd = 4, col = "black")

contour2D(z = V, levels = seq(0, 2, by = .05))
contour2D(V,x,y, levels = seq(0, 1, by = .1))

contour(x,y,V,nlevels = 500)

filled.contour(V,
               color.palette =
                 colorRampPalette(c("blue", "yellow","red"),
                                  space = "Lab"),
               asp = 1)


filled.contour(x = x, y = y, z = V, 
               plot.axes={ axis(1); axis(2); 
                 contour(x = x, y = y, z = V, add=T , nlevels = 1, level = 1, lwd = 4)}) 

