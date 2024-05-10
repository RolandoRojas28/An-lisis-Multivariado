# Seleccionar solo las columnas numéricas
numeric_cols <- sapply(universidad, is.numeric)
universidad_numeric <- universidad[, numeric_cols]
View(universidad_numeric)
#Matriz de correlacion
round(cor(x = universidad_numeric, method = "pearson"), 3)

library(psych)
multi.hist(x = universidad_numeric, dcol = c("green", "blue"), dlty = c("dotted", "solid"), main = "")
# linea verde es la forma que tiene la distribucion normal
# azul es la forma que tiene la distribucion de datos

#Grafico 2
library(GGally)
ggpairs(universidad_numeric, lower = list(continuous ="smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

#pasos a seguir 2
modelox = lm(universidad_numeric$Student_satisfaction ~ universidad_numeric$Founded_year + universidad_numeric$World_rank 
             + universidad_numeric$score + universidad_numeric$Minimum_IELTS_score + 
               universidad_numeric$fees + universidad_numeric$`Estimated_cost_of_living_per_year_(in_pounds)` + universidad_numeric$`Estimated_cost_of_living_per_year_(in_pounds)`)
#es como ponerle en una bolsa e identificar que variable servira para nuestro modelo

step(object = modelox, direction = "both", trace=1)
#las variables ganadoras q seran utiles para el modelo es: imc , edad, grosor de piel, con el codigo de arriba paso 2

# paso 2 de la siguiente diapositiva
# vamos a ver si existe colinealidad, ya vimos pero porsiacaso
library(car)
vif(modelox)
# el valor que tenemos es no hay de que preocuparnos, esta normal, sale 1 ,  tantos es normal
#paso 3 
# para ver el grafico
library(rgl)
# 3 variables 1y y 2x
plot3d(universidad_numeric$Student_satisfaction, universidad_numeric$Founded_year, universidad_numeric$`Estimated_cost_of_living_per_year_(in_pounds)`, pch = ".", size = 0.5)