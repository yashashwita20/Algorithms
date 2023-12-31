# Polynomial Regression and Piecewise Constant Fit




Loading Libraries

```r
library(stats)
library(matlib)
```

Creating canonical design matrix and computing the condition number.

```r
#data frame to store the condition number for different scenarios
cond_all <- data.frame(matrix(ncol = 0, nrow = 20))

#computing for different n values
for (n in c(30,50,100, 200,500,1000)){
  vec <- c()
  #x = runif(n,0,1)
  x = seq(1,n,1)/(n+1)
  
  for (p in seq(1,20,1)){
    #polynomial of degree p
    M = poly(x, p, raw = TRUE)
    
    #design matrix
    X = cbind(rep(1, n), M)
    svd_X = svd(X)

    #condition number
    cond = max(svd_X$d)/min(svd_X$d)
    vec <- c(vec,cond)
  }

  cond_all<-cbind(cond_all, vec)
}
```

Visualizing the results

```r
names(cond_all) = c(30,50,100,200,500,1000)

mypalette = rainbow(ncol(cond_all))

matplot(y = cond_all, type = 'l',xlab="p (degree of polynomial)",ylab="Condition Number",
        main="p vs condition number for various n values", lty = 1,
        col = mypalette,lwd=2)

legend(legend = paste("n=",colnames(cond_all)), x = "topleft", y = "topright",
       lty = 1, lwd = 2, col = mypalette)
```

![plot of chunk fig36](figure/fig36-1.png)


The condition number of a design matrix measures how much a change in the input affects the output. The matrix is ill-conditioned if the condition number is very large, such a matrix is almost singular and computing the linear system of equations will be prone to errors. We can observe from the graph that for a given n, the condition number increases with increasing p (degree of polynomial) and for a given p, the condition number decreases with increasing n.

## Function piecewiseConstant(x,y,L,plot=TRUE) to fit a piecewise constant model.

```r
piecewiseConstant <- function(x,y,L,plot=TRUE){ 
    #partitioning the unit interval
    K = seq(0,1,length.out=2^L+1)
    pts = rep(NA,2*2^L)
    val = rep(NA,2*2^L)

    for (j in 1:2^L){
        #conditional statement to consider the lowest value of x in the #first interval while fitting the model
        if (j==1){
            I = (K[j] <= x)&(x <= K[j+1]) } 
        else {
            I = (K[j] < x)&(x <= K[j+1])
        }

        #This conditional statement is to handle the case where no data points fall in an
        #interval. The previous interval line has been extended in such cases to make
        #the plot continuous i.e., {pts,val} of that interval take the values from
        #previous interval but if it is the first interval then we do not have a line to
        #extend, in that case we don't plot a line.
        if (len(y[I])==0){ 
            if(j!=1) {
                pts[2*j-1] = K[j]
                pts[2*j] = K[j+1]
                val[2*j-1] = val[2*(j-1)-1]
                val[2*j] = val[2*(j-1)]
            }
        } else {
            fit = lm(y[I] ~ 1)
            pts[2*j-1] = K[j]
            pts[2*j] = K[j+1]
            val[2*j-1] = coef(fit)
            val[2*j] = coef(fit)
        } 
    }
  
    #display plot if plot=TRUE else return the data points
    if (plot){
        plot(x, y, pch = 16, main="Piecewise constant fit", cex = 1) 
        lines(pts, val, col="red", lwd = 3)
    } else {
        return(list(pts = pts,val = val))
    } 
}
```

Loading Dataset

```r
load("data/04cars.rda")
#extracting required variables: City Mpg, Horsepower tmp = dat[,c(13,14)]
tmp = tmp[complete.cases(tmp),]
tmp = as.data.frame(tmp)
names(tmp) = c("hp", "mpg")
dat = tmp
attach(dat)

#normalizing hp
x = (hp-min(hp))/(max(hp)-min(hp))
y = mpg

l2_fit = piecewiseConstant(x,y,2,plot=FALSE)
l3_fit = piecewiseConstant(x,y,3,plot=FALSE)
l4_fit = piecewiseConstant(x,y,4,plot=FALSE)

plot(x, y, pch = 16, main="Piecewise constant fit", cex = 1)
lines(l2_fit$pts, l2_fit$val, col="blue", lwd = 3)
lines(l3_fit$pts, l3_fit$val, col="green", lwd = 3)
lines(l4_fit$pts, l4_fit$val, col="red", lwd = 3)
legend(x="topright", legend=c("L=2", "L=3","L=4"),col=c("blue", "green","red"),
       lty=1,lwd=3, cex=0.8)
```

![plot of chunk fig37](figure/fig37-1.png)
