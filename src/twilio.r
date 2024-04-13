install.packages("dotenv")
library(dotenv)

# Load environment variables from specific file
dotenv::load_dot_env(file="~/labo2024v1/src/twilio.env")

# Retrieve environment variables
SID <- Sys.getenv('TWILIO_ACCOUNT_SID')
if (SID == "") stop("TWILIO_ACCOUNT_SID not set in environment variables")

URL <- Sys.getenv('TWILIO_URL')
if (URL == "") stop("TWILIO_URL not set in environment variables")

TOKEN <- Sys.getenv('TWILIO_AUTH_TOKEN')
if (TOKEN == "") stop("TWILIO_AUTH_TOKEN not set in environment variables")

FROM <- Sys.getenv('TWILIO_FROM_PHONE_NUMBER')
if (FROM == "") stop("TWILIO_FROM_PHONE_NUMBER not set in environment variables")

TO <- Sys.getenv('TWILIO_TO_PHONE_NUMBER')
if (TO == "") stop("TWILIO_TO_PHONE_NUMBER not set in environment variables")

# Get all command line arguments
args <- commandArgs(trailingOnly = FALSE)

# Extract the script's filename and print it
# This assumes the script is invoked from the command line
script_filename <- sub("^--file=", "", args[grep("^--file=", args)])

MENSAJE <- paste("El script ", script_filename, " ha finalizado su ejecución.", sep = "")

wp <- function(mensaje=MENSAJE, sid=SID, token=TOKEN, from=FROM, to=TO, url=URL) {
  # cargo libreria requerida
  library(curl)
  
  # URL del API de Twilio
  url <- sprintf(url, sid)
  
  # Autenticación
  auth <- paste(sid, token, sep = ":")
  
  # Datos para enviar
  data <- list(
    To = to,
    From = from,
    Body = mensaje
  )
  
  # Prepara los datos para la codificación URL
  encoded_data <- sapply(names(data), function(key) {
    paste(key, curl::curl_escape(data[[key]]), sep = "=")
  }, USE.NAMES = FALSE)
  postfields <- paste(encoded_data, collapse = "&")
  
  # Configura y realiza la solicitud POST
  handle <- new_handle()
  handle_setopt(handle, userpwd = auth, postfields = postfields, httpheader = c('Content-Type: application/x-www-form-urlencoded'))
  response <- curl_fetch_memory(url, handle)
  response_content <- rawToChar(response$content)
}

