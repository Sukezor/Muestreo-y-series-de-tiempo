library(fpp2)
library(smooth)


datos2 <- Quiz_2_Muse2$Asistentes
datos2 <-ts(datos2)

modelodata <- sma_old(datos2, order= 3 , h= 2, silent= F)
summary(modelodata)

modelodata$forecast
modelodata$fitted
