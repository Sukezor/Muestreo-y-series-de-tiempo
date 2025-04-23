#install.packages("fpp2")
#install.packages("forecast")

library(forecast)
library(fpp2)

##Ajustar la serie

data(ausair) #Aparezca la serie en memoria temporal

print(ausair) #Para ver el inicio y final de la serie


## recortar la serie para que empiece en 1990 como indica el ejericio

ausair_cut= window(ausair, start= 1990)
ausair_cut

autoplot(ausair)
autoplot(ausair_cut)

#Es una serie con tendencia, estacionaria en varianza, no es estacionaria en 
##media, no tiene estacionalidad

modelo_holt= holt(ausair_cut, h=10, level=c(0.80, 0.90), alpha= 0.5, beta=0.4)
summary(modelo_holt)

##AIC 147.0974



modelo_holt2= holt(ausair_cut, h=10, level=c(0.80, 0.90))
summary(modelo_holt2)

plot(modelo_holt2)

plot(modelo_holt)

#Suavizamiento exponencial simple j

model_ses=ses(ausair_cut, h=10, level=c(0.80,0.90))

summary(model_ses)
autoplot(model_ses)


### El ganador es modelo_holt2, por AIC, por MAE y graficamente

-----------------------------------------------------------------------

  
data("austourist")
print(austourists) #Cuando no imprima, se puede imprimir automaticamente


start(austourists)
end(austourists)
frequency(austourists) #Cada cuanto se evalua la estacionalidad

autoplot(austourists)

#Análisis gráfico

#No es estacionario en media ni en varianza
##Tiene tendencia
##Tiene estacionalidad
#Su varianza está aumentando


Acf(austourists, lag.max = 40) #Nos permitirá saber la frecuencia de la serie

##Frecuencia es e número de períodos en los cuales se da la estacionalidad

findfrequency(austourists)  #No es tan efectivo, pero puede ser un complemento


###La frecuencia solo se utiliza en períodos estacionales

#Ajuste del modelo

modelo_holt=holt(austourists) #Los otros hiperparámetros son opcionales

autoplot(modelo_holt)


##holt winters aditivo


model_hw_a=hw(austourists, seasonal = "additive")

model_hw_a$model$aic
autoplot(model_hw_a)



model_hw_m=hw(austourists, seasonal = "multiplicative")

model_hw_m$model$aic
autoplot(model_hw_m)

checkresiduals(model_hw_m)









