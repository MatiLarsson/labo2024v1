# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(100019, 100043, 100049, 100057, 100069)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_binaria a partir del resto
  modelo <- rpart("clase_binaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con DOS columnas,
  #  llamadas "BAJA+2", y "BAJA+1 o CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing que es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_binaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5 # en Windows este valor debe ser 1
  )

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# reemplazo los "BAJA+1" y "CONTINUA" con "BAJA+1 o CONTINUA"
dataset$clase_ternaria[dataset$clase_ternaria %in% c("BAJA+1", "CONTINUA")] <- "BAJA+1 o CONTINUA"

# cambio el nombre de la columna
names(dataset)[names(dataset) == "clase_ternaria"] <- "clase_binaria"

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binaria_trial2.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( max_depth = integer(),
                              min_split = integer(),
                              minbucket = integer(),
                              cp = numeric(),
                              ganancia_promedio = numeric() )


# itero por los loops anidados para cada hiperparametro

# Por cada combinacion de hiperparametros se hace un llamado por cada semilla a ArbolesMontecarlo y se promedian las ganancias de las semillas (las cuales realizan cada una una particion diferente de train y test).
# Numero de modelos a construir = 6 * 1 * 11 * 11 * 5 (semillas) = 3630 modelos -> 726 5-fold ensembles de semillas

# vmax_depth
a <- c(6)
# cp
b <- c(-1)
# minbucket
c <- c(300, 550, 600, 650)
# vmin_split
d <- c(600, 1300, 1400)

iterations <- length(a) * length(b) * length(c) * length(d)
count <- 0

for (vmax_depth in a) {
  for (cp in b) {
    for (minbucket in c) {
      for (vmin_split in d) {

        # If vmin_split / 2 > minbucket, then minbucket = vmin_split / 2
        if (vmin_split < 2 * minbucket) {
          vmin_split <- 2 * minbucket
        }

        # notar como se agrega

        # vminsplit  minima cantidad de registros en un nodo para hacer el split
        param_basicos <- list(
          "cp" = cp, # complejidad minima
          "minsplit" = vmin_split,
          "minbucket" = minbucket, # minima cantidad de registros en una hoja
          "maxdepth" = vmax_depth # profundidad máxima del arbol
        ) 

        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

        # creo la tabla de resultados
        tb_grid_search <- rbindlist( 
          list( tb_grid_search, 
                list( vmax_depth, vmin_split, minbucket, cp, ganancia_promedio) ) )
        
        # Por cada 5-fold ensemble de semillas actualizo la cuenta de progreso y la edito en la consola
        count <- count + 1
        progress <- round((count / iterations) * 100, 2)
        cat("\rProgreso: ", progress, "%", sep = "")
        flush.console()
      }
    }
  }
  # escribo la tabla a disco en cada vuelta del loop mas externo
  Sys.sleep(2)  # espero un par de segundos

  fwrite( tb_grid_search,
          file = archivo_salida,
          sep = "\t" )
}

# envio un mensaje de whatsapp para notificar finalizacion del script
source("~/labo2024v1/src/rpart/twilio.r")
wp("Tu script ha finalizado")
