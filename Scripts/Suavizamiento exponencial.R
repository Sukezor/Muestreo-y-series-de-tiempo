#install.packages("fpp2")

library(fpp2) ### para los datos oil
library(forecast) ### paquete con los modelos ses, holt, y hw

data(oil) ### serie del paquete fpp2 quede en memoria

### si los datos están en formato serie de tiempo, no se necesitan configuración con la función TS

### analisis grafico

autoplot(oil) ## no es claro si tiene tendencia o no
Acf(oil) ### si parece tener tendencia
decompose(oil) ## no funciona para series con frequency=1

## la fequencia es el argumento que define cada cuantos periodos se evalúa la estacionalidad

print(oil) ## el print muestra características
### caundo no aparecen con los nombres se pueden mirar

start(oil)
end(oil)
frequency(oil)


mod_ses=ses(oil, h=20, level=c(0.6))
summary(mod_ses)


mod = ses(oil, h=20, level=c(0.6))
summary(mod)




# Demanda en 2014 sera de 500 millones, ¿P de que no pueda ser atendida?

mod_ses0=ses(oil, h=20, level=c(0.95,0.90), alpha = 0.1) #Modelo 1
summary(mod_ses0)

plot(mod_ses0) #Grafica 1
lines(mod_ses0$fitted, col="red", lty=2)


mod_ses1=ses(oil, h=20, level=c(0.95,0.90), alpha = 0.8) #Modelo 2
summary(mod_ses0)


plot(mod_ses1) #Grafica 2
lines(mod_ses1$fitted, col="blue", lty=2)


# Demanda en 2014 sera de 500 millones, ¿P de que no pueda ser atendida?

prediccion <- forecast(mod_ses, h = 1)  # h=1 para predecir el siguiente valor
prediccion


# Obtener los residuos del modelo
residuos <- mod_ses$residuals
residuos

# Calcular el error estándar de los residuos
error_estandar <- sd(residuos)
error_estandar

# Predicción del modelo para 2014 (el siguiente valor)
prediccion_2014 <- prediccion$mean
prediccion_2014

# Calcular el z-score para la demanda de 500 millones de toneladas
z_score <- (500 - prediccion_2014) / error_estandar

# Calcular la probabilidad de que la demanda real sea mayor que 500 millones de toneladas
probabilidad <- 1 - pnorm(z_score)  # pnorm() da la probabilidad acumulada de la distribución normal
probabilidad

















## caundo no se pone alpha, el modelo calcula el alpha que minimiza el AIC

##alpha = es el peso del último periodo en el promedio ponderado

# el complemento (1-alpha) ## es el peso que se distribuyen en todos los periodos hacia atras menos último

summary(mod_ses)
summary(mod_ses1)

mod_ses$mean ## pronosticos
mean(mod_ses$residuals) ## promedio de residuales es el me
mod_ses$upper ## intervalos de confianza superiores
mod_ses$lower ## intervalo de confianza inferiores
mod_ses$fitted ## predicciones en las mismas fechas de los datos originales 


plot(mod_ses) ## para sacar el grafico


library(smooth)
mod=sma_old(oil, silent=F, order=5)
summary(mod)


