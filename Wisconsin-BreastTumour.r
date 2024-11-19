#####Clasificacion utilizando KNN --------------------

##Clasificacion de cancer de mama ----

#Importamos el archivo .csv
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

#Examinamos la estructura del data frame
str(wbcd)

#Eliminamos la propiedad ID (no es necesaria para el entrenamiento)
wbcd <- wbcd[-1]

#Vemos la tabla con los diagnosticos
table(wbcd$diagnosis)

#Convertimos la propiedad diagnostico en factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

#Vemos los diagnosticos en porcentajes
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#Creamos la funcion de normalizacion
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Normalizamos los datos
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

#Corroboramos la normalizacion. Ahora tenemos valores comprendidos entre 0-1
summary(wbcd_n$area_mean)

#Creamos los sets de entrenamiento y testeo
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#Creamos las etiquetas para los sets de entrenamiento y testeo
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#Cargamos la libreria "class"
library(class)

#Algoritmo KNN
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

#Cargamos la liberia "gmodels"
library(gmodels)

#Creamos una tabla cruzada tabulada PREDICHO vs ACTUAL
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

#Probamos con distintos valores de K
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
