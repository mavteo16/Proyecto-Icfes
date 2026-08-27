# ============================================================
# PROYECTO ICFES: HOMOLOGACIÓN DE VARIABLES - BLOQUE HISTÓRICO (2010-2014-1)
# ============================================================
# Descripción: Lectura explícita de cohortes antiguas con 
# auditoría numérica integrada por cohorte.
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

library(data.table)

if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}

cat("============================================================\n")
cat("   INICIANDO HOMOLOGACIÓN FLEXIBLE CON AUDITORÍA NUMÉRICA   \n")
cat("============================================================\n\n")

variables_base <- c(
  "periodo", "estu_consecutivo","estu_genero" , "estu_estudiante",
  "cole_area_ubicacion", "cole_bilingue", "cole_calendario",
  "cole_jornada", "cole_naturaleza", "estu_depto_reside",
  "fami_estratovivienda", "fami_tieneinternet"
)

mapa_puntajes <- c(
  "recaf_punt_c_naturales"         = "punt_c_naturales",
  "recaf_punt_ingles"              = "punt_ingles",
  "recaf_punt_lectura_critica"     = "punt_lectura_critica",
  "recaf_punt_matematicas"         = "punt_matematicas",
  "recaf_punt_sociales_ciudadanas" = "punt_sociales_ciudadanas"
)

procesar_cohorte_historica <- function(nombre_periodo, ruta_archivo) {
  
  if (!file.exists(ruta_archivo)) {
    cat(sprintf("[AVISO] El archivo para el periodo %s no existe en la ruta: %s. Se omite.\n", nombre_periodo, ruta_archivo))
    return(NULL)
  }
  
  cat(sprintf("\n--------------------------------------------\n"))
  cat(sprintf("PROCESANDO COHORTE: %s\n", nombre_periodo))
  cat(sprintf("--------------------------------------------\n"))
  
  base_df <- fread(
    file = ruta_archivo, sep = ";", encoding = "Latin-1",
    quote = "\"", fill = Inf, header = TRUE,
    check.names = FALSE, strip.white = TRUE, data.table = TRUE
  )
  
  cat(sprintf("  -> Lectura exitosa. Filas: %s | Columnas: %s\n", 
              format(nrow(base_df), big.mark = ","), ncol(base_df)))
  
  # Verificación de variables base
  faltantes <- setdiff(variables_base, names(base_df))
  if (length(faltantes) > 0) {
    stop(sprintf("El proceso se detuvo en el periodo %s por variables base faltantes.", nombre_periodo))
  }
  
  # Tratamiento flexible de puntajes recaf
  for (orig in names(mapa_puntajes)) {
    dest <- mapa_puntajes[orig]
    if (orig %in% names(base_df)) {
      setnames(base_df, orig, dest)
    } else {
      # Asignación tipada correcta para evitar el warning de data.table
      base_df[, (dest) := NA_real_]
    }
  }
  
  variables_finales <- c(variables_base, unname(mapa_puntajes))
  base_homologada <- base_df[, ..variables_finales]
  
  # ==========================================================
  # AUDITORÍA NUMÉRICA INTERNA DE LA COHORTE
  # ==========================================================
  cat("  [Auditoría Numérica]:\n")
  cat(sprintf("    - Total registros guardados: %s\n", format(nrow(base_homologada), big.mark = ",")))
  cat(sprintf("    - Puntajes Matematicas válidos (no NA): %s\n", 
              format(sum(!is.na(suppressWarnings(as.numeric(base_homologada$punt_matematicas)))), big.mark = ",")))
  cat(sprintf("    - Puntajes Matematicas en NA (esperado en 2010/2011): %s\n", 
              format(sum(is.na(suppressWarnings(as.numeric(base_homologada$punt_matematicas)))), big.mark = ",")))
  
  # Exportar resultado homologado
  ruta_salida <- file.path(ruta_principal, sprintf("Saber11_%s_homologada.csv", nombre_periodo))
  fwrite(base_homologada, file = ruta_salida, sep = ";", bom = TRUE, na = "")
  
  cat(sprintf("  [¡ÉXITO!] Archivo generado: %s\n", basename(ruta_salida)))
}

# --- EJECUCIÓN ---
procesar_cohorte_historica("20101", file.path(ruta_principal, "Examen_Saber_11_20101.txt"))
procesar_cohorte_historica("20102", file.path(ruta_principal, "Examen_Saber_11_20102.txt"))
procesar_cohorte_historica("20111", file.path(ruta_principal, "Examen_Saber_11_20111.txt"))
procesar_cohorte_historica("20112", file.path(ruta_principal, "Examen_Saber_11_20112.txt"))
procesar_cohorte_historica("20121", file.path(ruta_principal, "Examen_Saber_11_20121.txt"))
procesar_cohorte_historica("20122", file.path(ruta_principal, "Examen_Saber_11_20122.txt"))
procesar_cohorte_historica("20131", file.path(ruta_principal, "Examen_Saber_11_20131.txt"))
procesar_cohorte_historica("20132", file.path(ruta_principal, "Examen_Saber_11_20132.txt"))
procesar_cohorte_historica("20141", file.path(ruta_principal, "Examen_Saber_11_20141.txt"))

cat("\n============================================\n")
cat("¡HOMOLOGACIÓN AUDITADA CON ÉXITO!\n")
cat("============================================\n")