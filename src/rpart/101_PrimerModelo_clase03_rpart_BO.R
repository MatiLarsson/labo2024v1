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

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

# Utilizo mis mejores hiperparametros hallados en el script 101_PrimerModelo_clase03_rpart_BO.R, en rankings 1, 2, 5, 10, 50:
#1 A) maxdepth = 9, minsplit = 930, minbucket = 465, cp = -0.669689956579172
#2 B) maxdepth = 9, minsplit = 926, minbucket = 461, cp = -0.537048054660702
#5 C) maxdepth = 8, minsplit = 906, minbucket = 404, cp = -0.668359523513742
#10 D) maxdepth = 9, minsplit = 770, minbucket = 321, cp = -0.186280863938737
#20 E) maxdepth = 3, minsplit = 931, minbucket = 465, cp = -0.299224011133662
#50 F) maxdepth = 8, minsplit = 6345, minbucket = 4, cp = -0.150148529286063

for (comb in c("A", "B", "C", "D", "E", "F")) {
    if (comb == "A") {
        maxdepth = 9
        minsplit = 930
        minbucket = 465
        cp = -0.669689956579172
    } else if (comb == "B") {
        maxdepth = 9
        minsplit = 926
        minbucket = 461
        cp = -0.537048054660702
    } else if (comb == "C") {
        maxdepth = 8
        minsplit = 906
        minbucket = 404
        cp = -0.668359523513742
    } else if (comb == "D") {
        maxdepth = 9
        minsplit = 770
        minbucket = 321
        cp = -0.186280863938737
    } else if (comb == "E") {
        maxdepth = 3
        minsplit = 931
        minbucket = 465
        cp = -0.299224011133662
    } else if (comb == "F") {
        maxdepth = 8
        minsplit = 6345
        minbucket = 4
        cp = -0.150148529286063
    }

    modelo <- rpart(
            formula = "clase_ternaria ~ .",
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

    # prediccion es una matriz con TRES columnas,
    # llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
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
        file = paste0("./exp/KA2001/K101_001_", comb, "_clase_03_posterior.csv"),
        sep = ","
    )
}

# envio un mensaje de whatsapp para notificar finalizacion del script
setwd("~/labo2024v1/src/rpart/") # Establezco el Working Directory
source("twilio.r")
wp("Tu script ha finalizado")
