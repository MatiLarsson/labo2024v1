# Corrida general del Workflow de semillerio

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")
require("ParamHelpers")

# creo environment global
envg <- env()

envg$EXPENV <- list()
envg$EXPENV$exp_dir <- "~/buckets/b1/exp/"
envg$EXPENV$wf_dir <- "~/buckets/b1/flow/"
envg$EXPENV$wf_dir_local <- "~/flow/"
envg$EXPENV$repo_dir <- "~/labo2024v1/"
envg$EXPENV$datasets_dir <- "~/buckets/b1/datasets/"
envg$EXPENV$arch_sem <- "mis_semillas.txt"

# default
envg$EXPENV$gcloud$RAM <- 624
envg$EXPENV$gcloud$cCPU <- 12

#------------------------------------------------------------------------------
# Error catching

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rabort.txt",
    append = TRUE 
    )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------
# inicializaciones varias

dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir_local, showWarnings = FALSE)
setwd( envg$EXPENV$wf_dir_local )

#------------------------------------------------------------------------------
# cargo la  "libreria" de los experimentos

exp_lib <- paste0( envg$EXPENV$repo_dir,"/src/lib/z590_exp_lib_01.r")
source( exp_lib )

#------------------------------------------------------------------------------

DT_incorporar_dataset_baseline <- function( pmyexp, parch, pserver="local")
{
  if( -1 == (param_local <- exp_init_datos( pmyexp, parch, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z511_DT_incorporar_dataset.r"

  param_local$primarykey <- c("numero_de_cliente", "foto_mes" )
  param_local$entity_id <- c("numero_de_cliente" )
  param_local$periodo <- c("foto_mes" )
  param_local$clase <- c("clase_ternaria" )

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Catastrophe Analysis  baseline

CA_catastrophe_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z521_CA_reparar_dataset.r"

  # Opciones MachineLearning EstadisticaClasica Ninguno
  param_local$metodo <- "MachineLearning" # MachineLearning EstadisticaClasica Ninguno

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Data Drifting baseline

DR_drifting_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/531_DR_corregir_drifting.r"

  # No me engraso las manos con Feature Engineering manual
  param_local$variables_intrames <- TRUE
  # valores posibles
  #  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
  param_local$metodo <- "rank_cero_fijo"

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# FE historia baseline

FE_historia_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z541_FE_historia.r"

  param_local$lag1 <- TRUE
  param_local$lag2 <- FALSE # no me engraso con los lags de orden 2
  param_local$lag3 <- FALSE # no me engraso con los lags de orden 3

  # baseline
  param_local$Tendencias1$run <- TRUE  # FALSE, no corre nada de lo que sigue
  param_local$Tendencias1$ventana <- 6
  param_local$Tendencias1$tendencia <- TRUE
  param_local$Tendencias1$minimo <- TRUE
  param_local$Tendencias1$maximo <- TRUE
  param_local$Tendencias1$promedio <- TRUE
  param_local$Tendencias1$ratioavg <- TRUE
  param_local$Tendencias1$ratiomax <- TRUE

  # baseline
  param_local$Tendencias2$run <- TRUE
  param_local$Tendencias2$ventana <- 3
  param_local$Tendencias2$tendencia <- TRUE
  param_local$Tendencias2$minimo <- TRUE
  param_local$Tendencias2$maximo <- TRUE
  param_local$Tendencias2$promedio <- TRUE
  param_local$Tendencias2$ratioavg <- TRUE
  param_local$Tendencias2$ratiomax <- TRUE


  # vaseline
  param_local$RandomForest$run <- TRUE
  param_local$RandomForest$num.trees <- 20
  param_local$RandomForest$max.depth <- 4
  param_local$RandomForest$min.node.size <- 1000
  param_local$RandomForest$mtry <- 40

  # varia de 0.0 a 2.0, si es 0.0 NO se activan
  param_local$CanaritosAsesinos$ratio <- 0.0
  # desvios estandar de la media, para el cutoff
  param_local$CanaritosAsesinos$desvios <- 4.0

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy baseline 202109

TS_strategy_baseline_202109 <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z551_TS_training_strategy.r"


  param_local$future <- c(202109)
  param_local$final_train <- c(202107, 202106, 202105, 202104, 202103, 202102, 202002, 202001, 201912, 201911, 201910, 201909, 201908, 201907, 201906, 201905)


  param_local$train$training <- c(202103, 202102, 202002, 202001, 201912, 201911, 201910, 201909, 201908, 201907, 201906, 201905, 201904, 201903, 201902, 201901)
  param_local$train$validation <- c(202106, 202105, 202104)
  param_local$train$testing <- c(202107)

  # undersampling  baseline
  param_local$train$undersampling <- 0.4

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy baseline  202107

TS_strategy_baseline_202107 <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z551_TS_training_strategy.r"


  param_local$future <- c(202107)
  param_local$final_train <- c(202105, 202104, 202103, 202102, 202101, 202012, 202011, 202010, 202009)


  param_local$train$training <- c(202103, 202102, 202101, 202012, 202011, 202010, 202009, 202008, 202007)
  param_local$train$validation <- c(202104)
  param_local$train$testing <- c(202105)

  # undersampling  baseline
  param_local$train$undersampling <- 0.2

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Hyperparamteter Tuning baseline

HT_tuning_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z561_HT_lightgbm.r"

  # En caso que se haga cross validation, se usa esta cantidad de folds
  param_local$lgb_crossvalidation_folds <- 5

  # Hiperparametros  del LightGBM
  #  los que tienen un solo valor son los que van fijos
  #  los que tienen un vector,  son los que participan de la Bayesian Optimization
  
  param_local$lgb_param <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds

    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0

    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

    extra_trees = FALSE,
    # Quasi  baseline, el minimo learning_rate es 0.02 !!
    learning_rate = c( 0.08, 0.25 ),
    feature_fraction = c( 0.35, 0.65 ),
    num_leaves = c( 500L, 1500L,  "integer" ),
    min_data_in_leaf = c( 10L, 400L, "integer" )
  )


  # una Beyesian de Guantes Blancos, solo hace 15 iteraciones
  param_local$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# proceso ZZ_final  baseline

ZZ_final_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z571_ZZ_final.r"

  # Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
  param_local$modelos_rank <- c(1)

  param_local$kaggle$envios_desde <-  9500L
  param_local$kaggle$envios_hasta <- 16000L
  param_local$kaggle$envios_salto <-   500L

  # para el caso que deba graficar
  param_local$graficar$envios_desde <-  8000L
  param_local$graficar$envios_hasta <- 20000L
  param_local$graficar$ventana_suavizado <- 2001L

  # Una corrida de Guantes Blancos solo usa 5 semillas
  param_local$qsemillas <- 5

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# proceso ZZ_final  baseline

ZZ_final_semillerio_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z881_ZZ_final_semillerio.r"

  # Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
  param_local$modelos_rank <- c(1)

  param_local$kaggle$envios_desde <-  9500L
  param_local$kaggle$envios_hasta <- 16000L
  param_local$kaggle$envios_salto <-   500L

  # para el caso que deba graficar
  param_local$graficar$envios_desde <-  8000L
  param_local$graficar$envios_hasta <- 20000L
  param_local$graficar$ventana_suavizado <- 2001L

  # El parametro fundamental de semillerio
  # Es la cantidad de LightGBM's que ensamblo
  param_local$semillerio <- 300

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# A partir de ahora comienza la seccion de Workflows Completos
#------------------------------------------------------------------------------
# Este es el  Workflow baseline con semillerio
# Que predice 202109
# y ya genera archivos para Kaggle

corrida_baseline_semillerio_202109 <- function( pnombrewf, suffix, pvirgen=FALSE )
{
  if( -1 == exp_wf_init( pnombrewf, pvirgen) ) return(0) # linea fija

  DT_incorporar_dataset_baseline( paste0("DT-1-sem-", suffix), "competencia_2024.csv.gz")
  CA_catastrophe_baseline( paste0("CA-1-sem-", suffix), paste0("DT-1-sem-", suffix) )

  DR_drifting_baseline( paste0("DR-1-sem-", suffix), paste0("CA-1-sem-", suffix) )
  FE_historia_baseline( paste0("FE-1-sem-", suffix), paste0("DR-1-sem-", suffix) )

  TS_strategy_baseline_202109( paste0("TS-1-sem-", suffix), paste0("FE-1-sem-", suffix) )

  HT_tuning_baseline( paste0("HT-1-sem-", suffix), paste0("TS-1-sem-", suffix) )

  # El ZZ depente de HT y TS
  ZZ_final_semillerio_baseline( paste0("ZZ-1-sem-", suffix), c( paste0("HT-1-sem-", suffix), paste0("TS-1-sem-", suffix) ) )


  exp_wf_end( pnombrewf, pvirgen ) # linea fija
}
#------------------------------------------------------------------------------
# Este es el  Workflow baseline con semillerio
# Que predice 202107
# genera completas curvas de ganancia
#   NO genera archivos para Kaggle
# por favor notar como este script parte de FE0001


corrida_baseline_semillerio_202107 <- function( pnombrewf, suffix, pvirgen=FALSE )
{
  if( -1 == exp_wf_init( pnombrewf, pvirgen) ) return(0) # linea fija

  DT_incorporar_dataset_baseline( paste0("DT-1-sem-", suffix), "competencia_2024.csv.gz")
  CA_catastrophe_baseline( paste0("CA-1-sem-", suffix), paste0("DT-1-sem-", suffix) )

  DR_drifting_baseline( paste0("DR-1-sem-", suffix), paste0("CA-1-sem-", suffix) )
  FE_historia_baseline( paste0("FE-1-sem-", suffix), paste0("DR-1-sem-", suffix) )

  TS_strategy_baseline_202107( paste0("TS-2-sem-", suffix), paste0("FE-1-sem-", suffix) )

  HT_tuning_baseline( paste0("HT-2-sem-", suffix), paste0("TS-2-sem-", suffix) )

  # El ZZ depente de HT y TS
  ZZ_final_semillerio_baseline( paste0("ZZ-2-sem-", suffix), c( paste0("HT-2-sem-", suffix), paste0("TS-2-sem-", suffix) ) )


  exp_wf_end( pnombrewf, pvirgen ) # linea fija
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

# Nombre del experimento
exp_name <- "SAA"

corrida_baseline_semillerio_202109( paste0("basem01-", exp_name), exp_name )


# Luego partiendo de  FE0001
# genero TS0002, HT0002 y ZZ0002

corrida_baseline_semillerio_202107( paste0("basem02-", exp_name), exp_name )

 