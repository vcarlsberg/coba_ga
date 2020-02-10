library(GA)
library(Metrics)



y<-data.frame(y=c(12838.14,22142.69 ,15023.77,
                  21058.53 ,344370.96 ,141714.70 ,
                  1275.97 ,1337.13 ,1565.52 ,
                  4599.40 , 9902.81 ,23409.75 ))
x1<-data.frame(y=c(0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5))
x2<-data.frame(y=c(2,4,6,8,10,12,14,16,18,20))
rmse(y[["y"]],x1[["y"]])
weight_kecil<-function(w1,w2) 
{
rmse(y,w1*x1+w2*x2)
}

GA <- ga(type = "real-valued", 
         fitness = function(x) weight_kecil(x[1],x[2]),
         lower =  c(-10,10), upper = c(-10,10))
summary(GA)

w1<-3
fit<-w1*y

f <- function(x)  (x^2+x)*cos(x)
lbound <- -10; ubound <- 10
curve(f, from = lbound, to = ubound, n = 1000)
GA <- ga(type = "real-valued", fitness = f, lower = c(th = lbound), upper = ubound)

curve(f, from = lbound, to = ubound, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)
