##################################
#                                #
#      SOLUCION PREGUNTA 1       #
##################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o información cargada
rm(list = ls()) 


# Cargando los paquetes
install.packages(c("missForest","ggplot2"))
install.packages("lattice")
install.packages("colorspace")
install.packages("data.table")
install.packages("randomForest")
install.packages("foreach")
install.packages("itertools")
install.packages("iterators")
install.packages("grid")
install.packages("DataExplorer")
install.packages("VIM")
install.packages("caret")
install.packages("dummies")
install.packages("dplyr")

library(lattice)
library(colorspace)
library(data.table)
library(randomForest)
library(foreach)
library(itertools)
library(iterators)
library(ggplot2)
library(caret)
library(DataExplorer)
library(VIM)
library(missForest)
library(ggplot2)
library(dummies)
library(foreign)
library(dplyr)

setwd("J:/CICLO 2018-II/ECO II/PC1 - 2018-II") 
getwd() 



datos <-read.spss("PROBLEMA 1.sav",
                  use.value.labels=TRUE, 
                  to.data.frame=TRUE)
# Viendo la estructura de los datos

str(datos) 
head(datos) 
dim(datos) 

glimpse(datos)


#####################################
#### ANALISIS DE DATOS PERDIDOS ####
#####################################

#----------------------------------------------------------
# Verificación de datos perdidos

library(DataExplorer)
plot_missing(datos) #Muestra los porcentajes de datos perdido por variable
#En la grafica te muestra la distribucion portual de los datos perdidos

#Histogramas de todas las variables
#En el histograma hay que mirar la distribucion y el tipo de variable
hist(datos$IR01, main = "AMABILIDAD AL MOMENTO DE INGRESAR RECLAMOS",col = "orange")
hist(datos$IR02, main = "INTERES DEL FUNCIONARIO AL MOMENTO DE INGRESAR EL RECLAMO",col = "orange")
hist(datos$IR03, main = "RAPIDEZ DEL FUNCIONARIO AL MOMENTO DE INGRESAR EL RECLAMO",col = "orange")
hist(datos$IR04, main = "FACILIDAD Y SIMPLEZA DEL FUNCIONARIO AL MOMENTO DE INGRESAR EL RECLAMO",col = "orange")
hist(datos$IR05, main = "NIVEL DE CONOCIMEINTOS DEL FUNCIONARIO AL MOMENTO DE INGRESAR EL RELCAMO",col = "orange")
hist(datos$IR06, main = "CANTIDAD DE INFORMACION BRINDADA AL MOMENTO DE INGRESAR EL RECLAMO",col = "orange")
hist(datos$IR07, main = "TIEMPO DE ESPERA PARA SER ATENDIDO AL MOMENTO DE INGRESAR EL RECLAMO",col = "orange")
hist(datos$IR08, main = "TIEMPO DE ATENCION AL MOMENTO DE INGRESAR EL RECLAMO",col = "orange")
hist(datos$RR01, main = "CANTIDAD DE INFORMACION Y SUSTENTO EN LA RESPUESTA DEL RECLAMO",col = "orange")
hist(datos$RR02, main = "RAPIDEZ EN LA RESPUESTA DEL RECLAMO",col = "orange")
hist(datos$RR03, main = "AMABILIDAD MOSTRADA EN LA RESPUESTA DEL RECLAMO",col = "orange")
hist(datos$RR04, main = "SEGURIDAD Y SENCILLEZ EN LA RESPUESTA DEL RECLAMO",col = "orange")
hist(datos$SAT_1, main = "SATISFACCION POR EL INGRSO DE RECLAMOS",col = "orange")
hist(datos$SAT_2, main = "SATISFACCION POR LA RESPUESTA DEL RECLAMO",col = "orange")
hist(datos$SAT, main = "SATISFACCION POR EL PROCESO DE RECLAMO",col = "orange")


library(dplyr)
#Se hace una seleecion de la variables
Datos_Final <- select(datos,IR01,IR02,IR03,IR04,IR05,IR06,IR07,IR08,RR01,RR02,RR03,RR04,SAT_1,SAT_2,SAT)
glimpse(Datos_Final) #Revisando la estructura de la data


###########################################
####### MODELO DE REGRESION LINEAL ########
###########################################


#Corriendo el primer modelo de regresion
library(MASS)
#Desarrollo del modelo lineal
resultado_1 <- lm(SAT ~ 
                    IR01+IR02+IR03+IR04+IR05+IR06+IR07+IR08+RR01+
                    RR02+RR03+RR04+SAT_1+SAT_2,data=Datos_Final)

summary(resultado_1)

#Tabla de Análisis de Varianzas del primer modelo de regresion
AnalVar_1 <- aov(SAT ~ 
                   IR01+IR02+IR03+IR04+IR05+IR06+IR07+IR08+RR01+
                   RR02+RR03+RR04+SAT_1+SAT_2,data=Datos_Final)

summary(AnalVar_1)

#Quitando las variables que nos son significativas segun el analisis de varianza inicial

#Desarrollo del modelo lineal
resultado_2 <- lm(SAT ~ 
                    IR01+IR02+IR03+IR04+IR05+IR06+IR07+RR01+
                    RR02+RR03+SAT_1,data=Datos_Final)

summary(resultado_2)


#Quitando las variables que nos son significativas segun el MODELO LM INICIAL

#Desarrollo del modelo lineal
resultado_3 <- lm(SAT ~ 
                    IR04+RR01+
                    RR02+RR03+SAT_1,data=Datos_Final)

summary(resultado_3)

#Tabla de Análisis de Varianzas del primer modelo de regresion
AnalVar_3 <- aov(SAT ~ 
                   IR04+RR01+
                   RR02+RR03+SAT_1,data=Datos_Final)

summary(AnalVar_3)

#COMPARAMOS Y NOS QUEDAMOS CON

coeficientes_3 <- as.data.frame(resultado_3$coefficients) #Coeficientes 
error_modelo_3 <- as.data.frame(resultado_3$residuals) #Valores residuales 
valor_ajustado_3 <- as.data.frame(resultado_3$fitted.values) #Valores ajustados 



#################### MEJOR MODELO ####################


#En el primer modelo se muestra la presencia de variables no significativas,
#por ende se correra modelos usando metodos de seleccion de variables
install.packages("leaps")
library(leaps)

mejores_modelos <- regsubsets(SAT ~., data=datos[-1], nvmax = 30)
summary(mejores_modelos)
names(summary(mejores_modelos))
summary(mejores_modelos)$adjr2 #R cuadrado ajustado
which.max(summary(mejores_modelos)$adjr2)

mejores_modelos <- regsubsets(SAT ~., data=datos[-1], nvmax = 30)
summary(mejores_modelos)
names(summary(mejores_modelos))
summary(mejores_modelos)$bic #R cuadrado ajustado
which.min(summary(mejores_modelos)$bic)

#Grafica de los distintos R Cuadrados ajustados generados por los distintos modelos hasta encontrar el mejor modelo
library(ggplot2)
p <- ggplot(data = data.frame(n_predictores = 1:15,
                              R_ajustado = summary(mejores_modelos)$adjr2),
            aes(x = n_predictores, y = R_ajustado)) +
  geom_line() +
  geom_point()

#Se identifica en rojo el máximo
p <- p + geom_point(aes(
  x = n_predictores[which.max(summary(mejores_modelos)$adjr2)],
  y = R_ajustado[which.max(summary(mejores_modelos)$adjr2)]),
  colour = "red", size = 3)
p <- p +  scale_x_continuous(breaks = c(0:30)) + 
  theme_bw() +
  labs(title = 'R2_ajustado vs número de predictores', 
       x =  'número predictores')
p

coef(object = mejores_modelos, id = 18)

summary(mejores_modelos)$bic[18]
glimpse(data_modelo)

resultado_mejor <- lm(T_Ingreso ~
                        CantTXN + T_RFM_LEA + T_GEN_Y + T_GEN_X + T_GEN_BABY + T_Sexo_M + T_Civ_SOLT + T_ZONA_1 + 
                        T_ZONA_2 + T_ZONA_3 + T_ZONA_4 + T_ZONA_5 + T_ZONA_7 + T_ZONA_8 + T_ZONA_9 + T_ZONA_10 + 
                        T_ZONA_11 + T_ZONA_12 + T_ZONA_13 + T_ZONA_14 + T_ZONA_16 + CantProd + T_ImpTXN + 
                        T_Linea_TC + T_Lin_TC_SBS + T_Deu_SBS,data=data_modelo)

summary(resultado_mejor)

#Tabla de Análisis de Varianzas del primer modelo de regresion
AnalVar_mejor <- aov(T_Ingreso ~
                       CantTXN + T_RFM_LEA + T_GEN_Y + T_GEN_X + T_GEN_BABY + T_Sexo_M + T_Civ_SOLT + T_ZONA_1 + 
                       T_ZONA_2 + T_ZONA_3 + T_ZONA_4 + T_ZONA_5 + T_ZONA_7 + T_ZONA_8 + T_ZONA_9 + T_ZONA_10 + 
                       T_ZONA_11 + T_ZONA_12 + T_ZONA_13 + T_ZONA_14 + T_ZONA_16 + CantProd + T_ImpTXN + 
                       T_Linea_TC + T_Lin_TC_SBS + T_Deu_SBS,data=data_modelo)

summary(AnalVar_mejor)


#Ajustando el modelo
resultado_mejor <- lm(T_Ingreso ~
                        CantTXN + T_RFM_LEA + T_GEN_X + T_Sexo_M + T_Civ_SOLT + T_ZONA_1 + 
                        T_ZONA_2 + T_ZONA_3 + T_ZONA_4 + T_ZONA_5 + T_ZONA_7 + T_ZONA_8 + T_ZONA_10 + 
                        T_ZONA_11 + T_ZONA_12 + T_ZONA_13 + T_ZONA_16 + CantProd + T_ImpTXN + 
                        T_Linea_TC + T_Lin_TC_SBS + T_Deu_SBS,data=data_modelo)

summary(resultado_mejor)

#Tabla de Análisis de Varianzas del primer modelo de regresion
AnalVar_mejor <- aov(T_Ingreso ~
                       CantTXN + T_RFM_LEA + T_GEN_X + T_Sexo_M + T_Civ_SOLT + T_ZONA_1 + 
                       T_ZONA_2 + T_ZONA_3 + T_ZONA_4 + T_ZONA_5 + T_ZONA_7 + T_ZONA_8 + T_ZONA_10 + 
                       T_ZONA_11 + T_ZONA_12 + T_ZONA_13 + T_ZONA_16 + CantProd + T_ImpTXN + 
                       T_Linea_TC + T_Lin_TC_SBS + T_Deu_SBS,data=data_modelo)

summary(AnalVar_mejor)


#################### VALIDACION DEL MODELO ####################


#Normalidad y outlier
residuos<-rstandard(resultado_mejor) # residuos estándares del modelo ajustado (completo) 
par(mfrow=c(1,3)) # divide la ventana en una fila y tres columnas 
hist(residuos) # histograma de los residuos estandarizados 
boxplot(residuos) # diagrama de cajas de los residuos estandarizados 
qqnorm(residuos) # gráfico de cuantiles de los residuos estandarizados 
qqline(residuos)  
par(mfrow=c(1,1)) # devuelve la pantalla a su estado original

#Deteccion de valores outlier a traves de los valores studentizados de los residuaos
library(dplyr)
studentized_residual <- rstudent(resultado_mejor)
ggplot(data = data_modelo[-1], aes(x = predict(resultado_mejor), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos studentized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(studentized_residual) > 3)


#Heterocedasticidad
plot(fitted.values(resultado_mejor),rstandard(resultado_mejor), xlab="Valores ajustados", ylab="Residuos estandarizados")  # gráfico 2D de los valores ajustados vs. los residuos estandarizados 
abline(h=0) # dibuja la recta en cero 

#Autocorrelacion de residual
install.packages("car")
library(car)
dwt(resultado_mejor, alternative = "two.sided")

#Medidas de influencia sobre los betas
summary(influence.measures(resultado_mejor)) 
influencePlot(resultado_mejor)




