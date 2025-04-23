library(smooth)
library(fpp2)
library(forecast) 

url= "https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/d_cerveza.csv"
datos=read.csv(url)

datos<- datos$demanda_cerveza

datos<- ts(datos, start=c(1980,1), frequency = 12)
datos<- ts(datos, end=c(1984,8), frequency = 12)



plot(datos, main="Demanda mensual de cerveza", ylab="Millones de litros")

acf(datos, lag.max = 40)


print(datos)

plot(datos)
Acf(datos, main="Autocorrelación (ACF) - Demanda de cerveza")
plot(decompose(datos))




modelo_hw = hw(datos, seasonal="additive",h=4, level=c(0.9, 0.1,0.2))
summary(modelo_hw)

autoplot(modelo_hw)
summary(modelo_hw)

# Ver los valores pronosticados
predicciones <- modelo_hw$mean
predicciones

