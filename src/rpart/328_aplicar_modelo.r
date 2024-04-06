# Generacion de archivo para Kaggle
# clase_binaria2  POS = { BAJA+1, BAJA+2 }
# se usan weights

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# cargar aqui sus parametros ganadores de la 
# optimización de hiperparametros
PARAM <- list()

# 1) minsplit=1437, minbucket=407, maxdepth=13, peso_positivos=22.9989592317802, corte=10737
# 2) minsplit=1479, minbucket=405, maxdepth=13, peso_positivos=22.9679255519513, corte=10599
# 5) minsplit=1446, minbucket=411, maxdepth=19, peso_positivos=22.8233547306751, corte=10873
# 10) minsplit=1433, minbucket=391, maxdepth=17, peso_positivos=24.9012018282469, corte=10981
# 50) minsplit=1038, minbucket=298, maxdepth=13, peso_positivos=19.2162844449898, corte=10788
# 100) minsplit=1487, minbucket=671, maxdepth=13, peso_positivos=48.5315886549905, corte=9771

# Resultados de la optimizacion BO:

PARAM$minsplit <- 1487
PARAM$minbucket <- 671
PARAM$maxdepth <- 13
PARAM$peso_positivos <- 48.5315886549905
envios <- 9771
rank <- 100

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# defino la clase_binaria2  POS = {BAJA+1, BAJA+2}
dtrain[, clase_binaria2 := 
  ifelse(clase_ternaria=="CONTINUA", "NEG", "POS" )]

campos_buenos <- setdiff( colnames(dtrain), c( "clase_ternaria" ) )


# genero el modelo,  aqui se construye el arbol

# calculo los pesos
vpeso <<- copy(as.vector(
  dtrain[, ifelse( clase_binaria2=="POS", PARAM$peso_positivos, 1.0)  ]))

# genero los parametros propios de rpart
param_rpart <- list()
param_rpart$cp <- -1.0  # lo dejo fijo
param_rpart$minsplit <- PARAM$minsplit
param_rpart$minbucket <- PARAM$minbucket
param_rpart$maxdepth <- PARAM$maxdepth


modelo <- rpart(
  formula = "clase_binaria2 ~ .",
  data = dtrain[, campos_buenos, with=FALSE], # los datos donde voy a entrenar
  xval = 0,
  control = param_rpart,
  weights = vpeso
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con DOS columnas,
# llamadas "POS", "NEG"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de POS
dapply[, prob_pos := prediccion[, "POS"]]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA3280")

# ordeno descendente por probabilidad
setorder( dapply, -prob_pos )

dapply[ , Predicted := 0L ]

# genero distintas salidas, una para cada corte
# debe ser recorrido en orden creciente


dapply[ 1:envios, Predicted := 1L]

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
  file = paste0( "./exp/KA3280/KA3280_001_", envios, "_", rank, ".csv"),
  sep = ","
)

