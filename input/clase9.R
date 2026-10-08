###################### -------- Clase 9 --------- ##############################

setwd("C:/Users/camil/Dropbox/PC/Documents/DOCENCIA/2026/Analisis Multinivel ISUC 2026/Clases/Clase 9")
library(car)
library(lme4)
library(texreg)
library(ggplot2)
library(dplyr)
library(haven)
library(influence.ME)

load("morgan2013.RData")
morgan=morgan2013
morgan$femaleC <- as.numeric(morgan$female)
morgan$employedC <- as.numeric(morgan$employed)


#########-------------- Análisis de supuestos a nivel 1-----------------########

library(lattice)

#Modelo
mn1 <-lmer(trustgov ~ age + educ + left + employed + leftpres + (left|country), data=morgan)
screenreg(m1, digits = 3)

par(mfrow=c(1,2))
hist(resid(mn1), main="Distribucion Residuos Nivel 1")
qqnorm(resid(mn1), main="Q-Q Plot Residuos Nivel 1", col="blue")

qqmath(ranef(mn1, condVar = FALSE)) 



#########----------- Análisis de observaciones influyentes--------------########

## Gráfico bivariado
theme_set(theme_light())
ggplot(morgan,aes(cm_educ, cm_poltrust))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x, se=F, size=0.5)+
  geom_text(label=morgan$country, 
            nudge_x = 0.0, nudge_y = 0.02, 
            check_overlap = T, size=3)+
  ylab("Confianza política (agregada)")+
  xlab("Nivel educacional (agregada)")

#Modelo
m1 <-lmer(trustgov ~ age + educ + left + employed + (1|country), data=morgan)
screenreg(m1, digits = 3)

## Df Betas
m1.influence<-influence(m1, "country")

dfbetas(m1.influence)
dfbetas(m1.influence, parameters = c(3))

#cutoff (2/raiz(J))
2/sqrt(19) #0.4588315

plot(m1.influence,
     which="dfbetas", cutoff = 0.458,
     xlab="DFbetaS", ylab="País", parameters = c(1:3))

plot(m1.influence,
     which="dfbetas", cutoff = 0.458,
     xlab="DFbetaS", ylab="País", parameters = c(4:5))


#Distancia de Cook
cooks.distance(m1.influence)

#cutoff
4/19 #0.2105263

plot(m1.influence,
     which="cook", cutoff = 0.2105)

#Significación estadística
sigtest(m1.influence, test=-1.96)$educ



## Ejercicio ##

## Utilizando la base de datos lapop23 (clase 6) estime un modelo para predecir la
#confianza en la policía (pt_police), como función de female, age, educR, ideo  e insegur. 
#También genere un cluster mean de promedio de inseguridad por país y agreguelo al 
#modelo como predictor de nivel 2.

#En base a este modelo, analice si existen observaciones de nivel 2 influyentes sobre
#los coeficientes y sobre la significación de los efectos.

library(tidyverse)
load("input/lapop23.RData")
colnames(lapop23)




