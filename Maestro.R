# ============================================================
# SCRIPT MAESTRO - PROYECTO LONGITUDINAL ICFES (2014-2025)
# Universidad del Magdalena - Estadística Industrial
# ============================================================
# Descripción: Controlador central. Define la ruta principal y 
# ejecuta de forma secuencial todos los scripts del proyecto.
# ============================================================

# Limpiar el entorno de trabajo por completo
rm(list = ls())

# Si la carpeta de resultados ya existe de una ejecución anterior, la eliminamos para empezar de cero
if (dir.exists("Resultados_Directos_S11_SPro")) {
  unlink("Resultados_Directos_S11_SPro", recursive = TRUE)
}

# ============================================================
# ZONA DE CONFIGURACIÓN ÚNICA
# ============================================================
# Define aquí la ruta de la carpeta donde tienes los archivos
ruta_principal <- "Datos" 
# ============================================================

cat("============================================================\n")
cat(" INICIANDO FLUJO AUTOMATIZADO - PROYECTO ICFES\n")
cat("============================================================\n\n")

tiempo_inicio <- Sys.time()

# ------------------------------------------------------------
# PASO 1: Homologación de cohorte especial 2014-1
# ------------------------------------------------------------
cat(">>> [PASO 1/7] Ejecutando: CambiarNombres20141.R...\n")
source("R/CambiarNombres20141.R")


# ------------------------------------------------------------
# PASO 2: Procesamiento y consolidación de Saber 11
# ------------------------------------------------------------
cat("\n>>> [PASO 2/7] Ejecutando: SABER11.R...\n")
source("R/SABER11.R")


# ------------------------------------------------------------
# PASO 3: Procesamiento y consolidación de Saber Pro
# ------------------------------------------------------------
cat("\n>>> [PASO 3/7] Ejecutando: SABERPRO.R...\n")
source("R/SABERPRO.R")


# ------------------------------------------------------------
# PASO 4: Cruces, filtrado temporal y cálculo de rezagos
# ------------------------------------------------------------
cat("\n>>> [PASO 4/7] Ejecutando: CRUCES.R...\n")
source("R/CRUCES.R")


# ------------------------------------------------------------
# PASO 5: Unificación de bases y ordenamiento por bloques
# ------------------------------------------------------------
cat("\n>>> [PASO 5/7] Ejecutando: UNIFICACION.R...\n")
source("R/UNIFICACION.R")


# ------------------------------------------------------------
# PASO 6: Detección, conteo y eliminación de anomalías numéricas
# ------------------------------------------------------------
cat("\n>>> [PASO 6/7] Ejecutando: detectar_anomalias.R...\n")
source("R/detectar_anomalias.R")


# ------------------------------------------------------------
# PASO 7: Limpieza final, estandarización y validación matemática
# ------------------------------------------------------------
cat("\n>>> [PASO 7/7] Ejecutando: LIMPIEZAFINAL.R...\n")
source("R/LIMPIEZAFINAL.R")


# Cálculo del tiempo total de ejecución
tiempo_fin <- Sys.time()
tiempo_total <- round(tiempo_fin - tiempo_inicio, 2)

cat("\n============================================================\n")
cat(" ¡FLUJO COMPLETADO CON ÉXITO DE PRINCIPIO A FIN!\n")
cat(sprintf(" Tiempo total de procesamiento: %s segundos\n", tiempo_total))
cat("============================================================\n")
