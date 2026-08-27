# ============================================================
# PROYECTO ICFES: CONSOLIDACIÓN Y ESTANDARIZACIÓN SABER PRO
# VENTANA TEMPORAL: 2014 - 2025
# ============================================================
# Descripción: Lectura, homologación, depuración y consolidación 
# de microdatos de Saber Pro con auditoría numérica integrada.
# ============================================================

paquetes_requeridos <- c("data.table", "stringi")

for (paquete in paquetes_requeridos) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

library(data.table)
library(stringi)

if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}


# ============================================================
# 2. DEFINICIÓN DE RUTAS DE LOS ARCHIVOS FUENTE
# ============================================================

rutas_archivos <- c(
  "2014" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2014.txt"),
  "2015" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2015.txt"), 
  "2016" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2016.txt"),
  "2017" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2017.txt"),
  "2018" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2018.txt"),
  "2019" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2019.txt"),
  "2020" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2020.txt"),
  "2021" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2021.txt"),
  "2022" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2022.txt"),
  "2023" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2023.txt"),
  "2024" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2024.txt")
)


# ============================================================
# 3. SELECCIÓN DE VARIABLES OBJETIVO
# ============================================================

variables_objetivo <- c(
  "periodo",
  "estu_consecutivo",
  "estu_genero",
  "estu_depto_reside",
  "fami_estratovivienda",
  "estu_nse_individual",
  "fami_educacionmadre",
  "fami_educacionpadre",
  "fami_ingreso_fmiliar_mensual",
  "fami_tieneinternet",
  "estu_trabaja_actualmente",
  "inst_nombre_institucion",
  "estu_prgm_academico",
  "mod_comuni_escrita_punt",
  "mod_competen_ciudada_punt",
  "mod_ingles_punt",
  "mod_lectura_critica_punt",
  "mod_razona_cuantitat_punt",
  "punt_global",
  "percentil_global"
)


# ============================================================
# 4. FUNCIONES AUXILIARES Y DE PROCESAMIENTO
# ============================================================

normalizar_nombre <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  return(x)
}

leer_archivo_icfes <- function(ruta) {
  if (!file.exists(ruta)) {
    stop(paste0("\nERROR CRÍTICO: No existe el archivo en la ruta:\n", ruta))
  }
  
  cat("\n--------------------------------------------\n")
  cat("LEYENDO ARCHIVO:\n", ruta, "\n")
  
  resultado <- tryCatch({
    fread(file = ruta, sep = ";", encoding = "UTF-8", quote = "\"", fill = Inf, header = TRUE,
          na.strings = c("", "NA", "N/A", "NULL", "null", "NaN", "nan"), strip.white = TRUE, showProgress = FALSE, data.table = TRUE, check.names = FALSE)
  }, error = function(e) { NULL })
  
  if (is.null(resultado)) {
    cat("  -> UTF-8 falló. Reintentando lectura con codificación Latin-1...\n")
    resultado <- tryCatch({
      fread(file = ruta, sep = ";", encoding = "Latin-1", quote = "\"", fill = Inf, header = TRUE,
            na.strings = c("", "NA", "N/A", "NULL", "null", "NaN", "nan"), strip.white = TRUE, showProgress = FALSE, data.table = TRUE, check.names = FALSE)
    }, error = function(e) {
      stop(paste0("\nFallo crítico al leer el archivo:\n", ruta, "\n\nDetalle:\n", e$message))
    })
  }
  
  names(resultado) <- trimws(names(resultado))
  cat("  -> Lectura exitosa. Filas:", nrow(resultado), "| Columnas:", ncol(resultado), "\n")
  return(resultado)
}

estandarizar_columnas <- function(dt, variables_objetivo) {
  nombres_originales <- names(dt)
  nombres_normalizados <- normalizar_nombre(nombres_originales)
  
  for (variable in variables_objetivo) {
    variable_normalizada <- normalizar_nombre(variable)
    indices <- which(nombres_normalizados == variable_normalizada)
    
    if (length(indices) == 1) {
      setnames(dt, old = nombres_originales[indices], new = variable)
    } else if (length(indices) > 1) {
      setnames(dt, old = nombres_originales[indices[1]], new = variable)
    }
  }
  return(dt)
}

completar_variables <- function(dt, variables_objetivo) {
  faltantes <- setdiff(variables_objetivo, names(dt))
  if (length(faltantes) > 0) {
    cat("  -> Variables ausentes en este periodo (completadas con NA):", paste(faltantes, collapse = ", "), "\n")
    for (variable in faltantes) {
      dt[, (variable) := NA_character_]
    }
  }
  return(dt)
}


# ============================================================
# 5. PROCESAMIENTO E ITERACIÓN POR CADA ARCHIVO
# ============================================================

lista_bases <- vector(mode = "list", length = length(rutas_archivos))
names(lista_bases) <- names(rutas_archivos)

for (nombre_archivo in names(rutas_archivos)) {
  ruta <- rutas_archivos[[nombre_archivo]]
  cat("\n============================================\n")
  cat("PROCESANDO COHORTE / AÑO:", nombre_archivo, "\n")
  cat("============================================\n")
  
  dt <- leer_archivo_icfes(ruta)
  dt <- estandarizar_columnas(dt, variables_objetivo)
  dt <- completar_variables(dt, variables_objetivo)
  dt <- dt[, ..variables_objetivo]
  
  lista_bases[[nombre_archivo]] <- dt
}


# ============================================================
# 6. CONSOLIDACIÓN DE LAS BASES
# ============================================================

cat("\n============================================\n")
cat("CONSOLIDANDO TODAS LAS BASES SABER PRO...\n")
cat("============================================\n")

base_consolidada <- rbindlist(lista_bases, use.names = TRUE, fill = TRUE)

cat("Consolidación inicial completada. Registros totales:", nrow(base_consolidada), "\n")


# ============================================================
# 7. LIMPIEZA DE IDENTIFICACIÓN Y PERÍODO
# ============================================================

base_consolidada[, estu_consecutivo := trimws(as.character(estu_consecutivo))]
base_consolidada[, estu_consecutivo := fifelse(estu_consecutivo == "", NA_character_, estu_consecutivo)]

base_consolidada[, periodo := trimws(as.character(periodo))]
base_consolidada[, periodo := fifelse(periodo == "", NA_character_, periodo)]


# ============================================================
# 8. FILTRO TEMPORAL ESTRICTO (2014–2025)
# ============================================================

base_consolidada[, anio_periodo := suppressWarnings(as.integer(substr(periodo, 1, 4)))]

filas_antes_filtro <- nrow(base_consolidada)

base_consolidada <- base_consolidada[
  !is.na(anio_periodo) &
    anio_periodo >= 2014 &
    anio_periodo <= 2025
]

filas_despues_filtro <- nrow(base_consolidada)

cat("\n============================================\n")
cat("FILTRO TEMPORAL APLICADO (2014 - 2025)\n")
cat("============================================\n")
cat("Registros antes del filtro:", filas_antes_filtro, "\n")
cat("Registros después del filtro:", filas_despues_filtro, "\n")
cat("Registros descartados fuera de rango:", filas_antes_filtro - filas_despues_filtro, "\n")


# ============================================================
# 9. ORDENAMIENTO Y AUDITORÍA NUMÉRICA
# ============================================================

base_consolidada[, anio_periodo := NULL]
setcolorder(base_consolidada, variables_objetivo)
base_final <- base_consolidada

cat("\n============================================\n")
cat("REPORTE DE AUDITORÍA NUMÉRICA - SABER PRO\n")
cat("============================================\n")
cat("  - Conteo de valores NA en 'estu_consecutivo':", sum(is.na(base_final$estu_consecutivo)), "\n")
cat("  - Conteo de valores NA en 'periodo':", sum(is.na(base_final$periodo)), "\n")

# Distribución cuantitativa exacta por periodo en la base final
resumen_sp_periodo <- base_final[, .(
  Registros = .N,
  Puntajes_Globales_Validos = sum(!is.na(suppressWarnings(as.numeric(punt_global))))
), by = periodo]
setorder(resumen_sp_periodo, periodo)

cat("\n[Auditoría Numérica] Distribución por período en Saber Pro:\n")
print(resumen_sp_periodo)


# ============================================================
# 10. EXPORTACIÓN DEL ARCHIVO CONSOLIDADO
# ============================================================

ruta_salida <- file.path(ruta_principal, "SABER PRO COMPLETO.csv")

cat("\nExportando base consolidada a disco...\n")
fwrite(base_final, file = ruta_salida, sep = ";", bom = TRUE, na = "")

cat("\n============================================\n")
cat("PROCESO DE SABER PRO FINALIZADO CORRECTAMENTE\n")
cat("============================================\n")
cat("Archivo guardado en:", ruta_salida, "\n")
cat("Total registros consolidados:", nrow(base_final), "\n")