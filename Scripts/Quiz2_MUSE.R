library(smooth)
library(fpp2)

data <- Datos_SES_Quiz_2$Y_t
data<- ts(data)



modelo <- ses(data, h = 22, level = c(80, 95), alpha = 0.33) 
summary(modelo)

plot(modelo)
lines(modelo$fitted, col= "red", lty=2)

modelo$mean
modelo$fitted
