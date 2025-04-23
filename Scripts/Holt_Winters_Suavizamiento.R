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

