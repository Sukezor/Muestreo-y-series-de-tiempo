##Packages

library(forecast)
library(smooth)
library(Metrics) # para mape, mse, rmse

####

url = "https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/refs/heads/master/data/valor_accion_ecopetrol.csv"

datos = read.csv(url)
datos

### Convertir el csv en serie de tiempo

serie = ts(datos$valor, end = c(2025, 3), frequency = 12)
serie

par(mfrow = c(1,2))

#autoplot(serie)
plot(serie)
Acf(serie)
plot(decompose(serie))

## Es una serie estacionaria entonces funcionan bien las medias móviles

model_mm = sma_old(serie, h = 10, interval = "p", level = 0.95, silent = F)

summary(model_mm)

checkresiduals(model_mm)

model_mm$forecast
model_mm$lower
model_mm$upper

datospronos = data.frame(forecast = model_mm$forecast)
datospronos$int_inf = model_mm$lower
datospronos$int_sup = model_mm$upper

# AIC, BIC, MSE, RMSE los mejores calificado son los que más aproximados al cero estén

## Ventana de 2

model_mm2 = sma_old(serie, h = 10, interval = "p", level = 0.95, silent = F, order = 2)

summary(model_mm2)

checkresiduals(model_mm2)

mape(serie, model_mm$fitted) *100 #fitted son los valores ajustados del modelo
mae(serie, model_mm$fitted)
rmse(serie, model_mm$fitted)

s_est = serie - model_mm$fitted
mean(s_est)

mean(model_mm$residuals) #Forma para saber si se esta subestimando o sobreestimando






