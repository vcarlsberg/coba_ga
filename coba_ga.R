library(GA)
library(Metrics)



actual<-c(13222,30957,22331, 
          28502 ,502261 ,140682 ,
          1889 , 1298 , 3392 ,
          10641 ,17433 ,35651 )
hasil_arima<-c(39032.7903,31996.9776,30191.8589,
               33138.4266, 201557.3086, 464821.5256,  
               11219.9392,11446.2748,12878.6900,
               12248.0843,  14161.8256,  49010.8387)
hasil_nn<-c(35591.984,  30627.855,  27175.530,  
            33331.594, 214548.956, 454047.727,  
            11754.840,11527.078,  13048.687,  
            12097.891,  14518.818,  49273.576)

#yhat<-c(31404.2431,10957.7780,25483.1285,
#          17095.0213,130522.1165,55091.9926,    
#          638.2738,3266.3366,6681.7174,5494.0391,
#          152932.3290,26998.6242)
#x1<-c(0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3)
#x2<-c(2,4,6,8,10,12,14,16,18,20,22,24)
w1=1
w2=1
rmse(actual,w1*hasil_arima+w2*hasil_nn)

weight_kecil<-function(w1) 
{
  rmse(y,w1*hasil_arima+w1*hasil_nn)
}

GA <- ga(type = "real-valued", 
         fitness = function(w) weight_kecil(w[1]),
         lower =-0, upper = 100)
summary(GA)

w1<-3
fit<-w1*y

f <- function(x)  (x^2+x)*cos(x)
lbound <- -10; ubound <- 10
curve(f, from = lbound, to = ubound, n = 1000)
GA <- ga(type = "real-valued", fitness = f, lower = c(th = lbound), upper = ubound)

curve(f, from = lbound, to = ubound, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)
