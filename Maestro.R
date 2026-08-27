# ============================================================
# SCRIPT MAESTRO - PROYECTO LONGITUDINAL ICFES (2014-2025)
# Universidad del Magdalena - Estadística Industrial
# ============================================================
# Descripción: Controlador central. Define la ruta principal y 
# ejecuta de forma secuencial y automatizada todo el pipeline del proyecto,
# desde la ingestión de fuentes hasta el análisis inferencial y reportes.
# ============================================================

# Limpiar el entorno de trabajo por completo
rm(list = ls())

# Opcional: Si deseas borrar resultados previos para garantizar una ejecución limpia desde cero
# if (dir.exists("Resultados_Directos_S11_SPro")) {
#   unlink("Resultados_Directos_S11_SPro", recursive = TRUE)
# }

# ============================================================
# ZONA DE CONFIGURACIÓN ÚNICA
# ============================================================
ruta_principal <- "Datos" 
# ============================================================

cat("============================================================\n")
cat("   INICIANDO FLUJO AUTOMATIZADO INTEGRAL - PROYECTO ICFES   \n")
cat("============================================================\n\n")

tiempo_inicio <- Sys.time()

# ------------------------------------------------------------
# PASO 1: Homologación de cohorte especial 2014-1
# ------------------------------------------------------------
cat(">>> [PASO 1/9] Ejecutando: CambiarNombres20141.R...\n")
source("R/CambiarNombres20141.R")


# ------------------------------------------------------------
# PASO 2: Procesamiento y consolidación de Saber 11
# ------------------------------------------------------------
cat("\n>>> [PASO 2/9] Ejecutando: SABER11.R...\n")
source("R/SABER11.R")


# ------------------------------------------------------------
# PASO 3: Procesamiento y consolidación de Saber Pro
# ------------------------------------------------------------
cat("\n>>> [PASO 3/9] Ejecutando: SABERPRO.R...\n")
source("R/SABERPRO.R")


# ------------------------------------------------------------
# PASO 4: Cruces, filtrado temporal y cálculo de rezagos
# ------------------------------------------------------------
cat("\n>>> [PASO 4/9] Ejecutando: CRUCES.R...\n")
source("R/CRUCES.R")


# ------------------------------------------------------------
# PASO 5: Unificación de bases y ordenamiento por bloques
# ------------------------------------------------------------
cat("\n>>> [PASO 5/9] Ejecutando: UNIFICACION.R...\n")
source("R/UNIFICACION.R")


# ------------------------------------------------------------
# PASO 6: Detección, conteo y eliminación de anomalías numéricas
# ------------------------------------------------------------
cat("\n>>> [PASO 6/9] Ejecutando: ANOMALIAS.R...\n")
source("R/ANOMALIAS.R")


# ------------------------------------------------------------
# PASO 7: Limpieza final y depuración general de la base unificada
# ------------------------------------------------------------
cat("\n>>> [PASO 7/9] Ejecutando: LIMPIEZAFINAL.R...\n")
source("R/LIMPIEZAFINAL.R")


# ------------------------------------------------------------
# PASO 8: Generación de estadísticas descriptivas y exploratorias
# ------------------------------------------------------------
cat("\n>>> [PASO 8/9] Ejecutando: DESCRIPTIVAS.R...\n")
source("R/DESCRIPTIVAS.R")


# ------------------------------------------------------------
# PASO 9: Modelos econométricos, valor agregado, brechas y regiones
# ------------------------------------------------------------
cat("\n>>> [PASO 9/9] Ejecutando: INFERENCIAL.R...\n")
source("R/INFERENCIAL.R")


# Cálculo del tiempo total de ejecución
tiempo_fin <- Sys.time()
tiempo_total <- round(tiempo_fin - tiempo_inicio, 2)

cat("\n============================================================\n")
cat(" ¡FLUJO AUTOMATIZADO COMPLETADO CON ÉXITO DE PRINCIPIO A FIN!\n")
cat(sprintf(" Tiempo total de procesamiento: %.2f minutos\n", as.numeric(tiempo_total, units = "mins")))
cat("============================================================\n")