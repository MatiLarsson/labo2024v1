# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# reemplazo los "BAJA+1" y "CONTINUA" con "BAJA+1 o CONTINUA"
dataset$clase_ternaria[dataset$clase_ternaria %in% c("BAJA+1", "CONTINUA")] <- "BAJA+1 o CONTINUA"

# cambio el nombre de la columna
names(dataset)[names(dataset) == "clase_ternaria"] <- "clase_binaria"

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_binaria a partir de el resto de las variables

# Utilizo mis mejores hiperparametros hallados en el script 141_gridsearch_esqueleto_binaria2.r, en rankings 1, 2, 5, 10, 50, 100:
# A) maxdepth = 6, minsplit = 1500, minbucket = 600, cp = -1
# B) maxdepth = 6, minsplit = 1300, minbucket = 600, cp = -1
# C) maxdepth = 6, minsplit = 1300, minbucket = 650, cp = -1
# D) maxdepth = 6, minsplit = 1200, minbucket = 600, cp = -1
# E) maxdepth = 6, minsplit = 1600, minbucket = 800, cp = -1
# F) maxdepth = 14, minsplit = 3000, minbucket = 1500, cp = -1

for (comb in c("A", "B", "C", "D", "E", "F")) {
    if (comb == "A") {
        maxdepth = 6
        minsplit = 1500
        minbucket = 600
        cp = -1
    } else if (comb == "B") {
        maxdepth = 6
        minsplit = 1300
        minbucket = 600
        cp = -1
    } else if (comb == "C") {
        maxdepth = 6
        minsplit = 1300
        minbucket = 650
        cp = -1
    } else if (comb == "D") {
        maxdepth = 6
        minsplit = 1200
        minbucket = 600
        cp = -1
    } else if (comb == "E") {
        maxdepth = 6
        minsplit = 1600
        minbucket = 800
        cp = -1
    } else if (comb == "F") {
        maxdepth = 14
        minsplit = 3000
        minbucket = 1500
        cp = -1
    }

    modelo <- rpart(
            formula = "clase_binaria ~ .",
            data = dtrain, # los datos donde voy a entrenar
            xval = 0,
            cp = cp, # esto significa no limitar la complejidad de los splits
            minsplit = minsplit, # minima cantidad de registros para que se haga el split
            minbucket = minbucket, # tamaño minimo de una hoja
            maxdepth = maxdepth
    ) # profundidad maxima del arbol


    # aplico el modelo a los datos nuevos
    prediccion <- predict(
            object = modelo,
            newdata = dapply,
            type = "prob"
    )

    # prediccion es una matriz con DOS columnas,
    # llamadas "BAJA+1" y "BAJA+2 o CONTINUA"
    # cada columna es el vector de probabilidades

    # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
    dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

    # solo le envio estimulo a los registros
    #  con probabilidad de BAJA+2 mayor  a  1/40
    dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

    # genero el archivo para Kaggle
    # primero creo la carpeta donde va el experimento
    dir.create("./exp/")
    dir.create("./exp/KA2001")

    # solo los campos para Kaggle
    fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = paste0("./exp/KA2001/K101_001_binaria", comb, ".csv"),
        sep = ","
    )
}

# envio un mensaje de whatsapp para notificar finalizacion del script
source("~/labo2024v1/src/rpart/twilio.r")
wp("Tu script ha finalizado")
