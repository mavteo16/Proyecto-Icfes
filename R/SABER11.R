# ============================================================
# PROYECTO ICFES: CONSOLIDACIÓN Y ESTANDARIZACIÓN SABER 11
# VENTANA TEMPORAL: 2014 - 2025
# ============================================================
# Descripción: Este script lee, homologa, depura y consolida
# los microdatos históricos de la prueba Saber 11 para los 
# períodos comprendidos entre 2014 y 2025, garantizando una 
# estructura uniforme para los análisis posteriores.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y LIBRERÍAS
# ============================================================

# Verificación e instalación automática de dependencias necesarias
paquetes_requeridos <- c("data.table", "stringi")

for (paquete in paquetes_requeridos) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
}

library(data.table)
library(stringi)


# ============================================================
# 2. DEFINICIÓN DE RUTAS DE LOS ARCHIVOS FUENTE
# ============================================================

# Diccionario de rutas locales asociadas a cada período de evaluación
rutas_archivos <- c(
  "20141" = file.path(ruta_principal, "Saber11_20141_homologada.csv"),
  "20142" = file.path(ruta_principal, "Examen_Saber_11_20142.txt"),
  "20151" = file.path(ruta_principal, "Examen_Saber_11_20151.txt"),
  "20152" = file.path(ruta_principal, "Examen_Saber_11_20152.txt"),
  "20161" = file.path(ruta_principal, "Examen_Saber_11_20161.txt"),
  "20162" = file.path(ruta_principal, "Examen_Saber_11_20162.txt"),
  "20171" = file.path(ruta_principal, "Examen_Saber_11_20171.txt"),
  "20172" = file.path(ruta_principal, "Examen_Saber_11_20172.txt"),
  "20181" = file.path(ruta_principal, "Examen_Saber_11_20181.txt"),
  "20182" = file.path(ruta_principal, "Examen_Saber_11_20182.txt"),
  "20191" = file.path(ruta_principal, "Examen_Saber_11_20191.txt"),
  "20192" = file.path(ruta_principal, "Examen_Saber_11_20192 (1).txt"),
  "20201" = file.path(ruta_principal, "Examen_Saber_11_20201.txt"),
  "20202" = file.path(ruta_principal, "Examen_Saber_11_20202.txt"),
  "20211" = file.path(ruta_principal, "Examen_Saber_11_20211.txt"),
  "20212" = file.path(ruta_principal, "Examen_Saber_11_20212.txt"),
  "20221" = file.path(ruta_principal, "Examen_Saber_11_20221 (1).txt"),
  "20222" = file.path(ruta_principal, "Examen_Saber_11_20222 (1).txt"),
  "20231" = file.path(ruta_principal, "Examen_Saber_11_20231 (1).txt"),
  "20232" = file.path(ruta_principal, "Examen_Saber_11_20232.txt"),
  "20241" = file.path(ruta_principal, "Examen_Saber_11_20241.txt"),
  "20242" = file.path(ruta_principal, "Examen_Saber_11_20242.txt"),
  "20251" = file.path(ruta_principal, "Examen_Saber_11_20251.txt"),
  "20252" = file.path(ruta_principal, "Examen_Saber_11_20252.txt")
)


# ============================================================
# 3. SELECCIÓN DE VARIABLES OBJETIVO
# ============================================================

variables_objetivo <- c(
  "periodo",
  "estu_consecutivo",
  "estu_estudiante",
  "cole_area_ubicacion",
  "cole_bilingue",
  "cole_calendario",
  "cole_jornada",
  "cole_naturaleza",
  "estu_depto_reside",
  "estu_inse_individual",
  "estu_repite",
  "fami_estratovivienda",
  "fami_tieneinternet",
  "punt_c_naturales",
  "punt_ingles",
  "punt_lectura_critica",
  "punt_matematicas",
  "punt_sociales_ciudadanas",
  "percentil_global",
  "punt_global"
)


# ============================================================
# 4. FUNCIONES DE APOYO Y NORMALIZACIÓN
# ============================================================

normalizar_nombre <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  return(x)
}

leer_archivo_icfes <- function(ruta) {
  if (!file.exists(ruta)) {
    stop(paste0("\nArchivo no encontrado en la ruta especificada:\n", ruta))
  }
  
  cat("\nLeyendo archivo:", basename(ruta), "\n")
  
  resultado <- tryCatch({
    fread(
      file = ruta,
      sep = ";",
      encoding = "UTF-8",
      quote = "\"",
      fill = Inf,
      header = TRUE,
      na.strings = c("", "NA", "N/A", "NULL", "null", "NaN", "nan"),
      strip.white = TRUE,
      showProgress = FALSE,
      data.table = TRUE,
      check.names = FALSE
    )
  }, error = function(e) {
    NULL
  })
  
  if (is.null(resultado)) {
    resultado <- tryCatch({
      fread(
        file = ruta,
        sep = ";",
        encoding = "Latin-1",
        quote = "\"",
        fill = Inf,
        header = TRUE,
        na.strings = c("", "NA", "N/A", "NULL", "null", "NaN", "nan"),
        strip.white = TRUE,
        showProgress = FALSE,
        data.table = TRUE,
        check.names = FALSE
      )
    }, error = function(e) {
      stop(paste0("\nFallo crítico de lectura en:\n", ruta, "\n\nDetalle del error: ", e$message))
    })
  }
  
  names(resultado) <- trimws(names(resultado))
  cat("  -> Filas leídas:", nrow(resultado), "| Columnas:", ncol(resultado), "\n")
  
  return(resultado)
}

estandarizar_columnas <- function(dt, variables_objetivo) {
  nombres_originales <- names(dt)
  nombres_normalizados <- normalizar_nombre(nombres_originales)
  
  for (variable in variables_objetivo) {
    variable_normalizada <- normalizar_nombre(variable)
    indices <- which(nombres_normalizados == variable_normalizada)
    
    if (length(indices) == 1) {
      setnames(dt, nombres_originales[indices], variable)
    } else if (length(indices) > 1) {
      warning(paste0("Advertencia: Múltiples coincidencias detectadas para la variable '", variable, "'. Se conservará la primera."))
      setnames(dt, nombres_originales[indices[1]], variable)
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
# 5. PROCESAMIENTO E ITERACIÓN POR CADA PERÍODO
# ============================================================

lista_bases <- list()

for (periodo_archivo in names(rutas_archivos)) {
  
  cat("\n============================================\n")
  cat("PROCESANDO PERÍODO:", periodo_archivo, "\n")
  cat("============================================\n")
  
  ruta <- rutas_archivos[[periodo_archivo]]
  
  dt <- leer_archivo_icfes(ruta)
  dt <- estandarizar_columnas(dt, variables_objetivo)
  dt <- completar_variables(dt, variables_objetivo)
  dt <- dt[, ..variables_objetivo]
  
  lista_bases[[periodo_archivo]] <- dt
}


# ============================================================
# 6. CONSOLIDACIÓN DE LAS BASES
# ============================================================

cat("\n============================================\n")
cat("CONSOLIDANDO TODAS LAS COHORTES...\n")
cat("============================================\n")

base_final <- rbindlist(
  lista_bases,
  use.names = TRUE,
  fill = TRUE
)

cat("Consolidación inicial completada.\n")
cat("Registros totales:", nrow(base_final), "\n")
cat("Variables totales:", ncol(base_final), "\n")


# ============================================================
# 7. LIMPIEZA Y FILTRADO TEMPORAL (2014-2025)
# ============================================================

base_final[, estu_consecutivo := trimws(as.character(estu_consecutivo))]
base_final[estu_consecutivo == "", estu_consecutivo := NA_character_]

base_final[, periodo := trimws(as.character(periodo))]
base_final[periodo == "", periodo := NA_character_]

base_final[, anio_periodo := suppressWarnings(as.integer(substr(periodo, 1, 4)))]

filas_iniciales <- nrow(base_final)

base_final <- base_final[
  !is.na(anio_periodo) &
    anio_periodo >= 2014 &
    anio_periodo <= 2025
]

filas_finales <- nrow(base_final)
base_final[, anio_periodo := NULL]


# ============================================================
# 8. CONTROLES DE CALIDAD Y AUDITORÍA
# ============================================================

cat("\n============================================\n")
cat("REPORTE DE CONTROLES DE CALIDAD\n")
cat("============================================\n")

cat("Filas antes del filtro temporal:", filas_iniciales, "\n")
cat("Filas después del filtro temporal:", filas_finales, "\n")
cat("Registros descartados fuera de rango:", filas_iniciales - filas_finales, "\n\n")

faltantes_finales <- setdiff(variables_objetivo, names(base_final))

if (length(faltantes_finales) > 0) {
  stop(paste0("\nERROR CRÍTICO: FALTAN VARIABLES EN LA BASE FINAL:\n", paste(faltantes_finales, collapse = ", ")))
} else {
  cat("[OK] Verificación superada: Todas las variables de interés están presentes.\n")
}

cat("  - Conteo de 'estu_consecutivo' en NA:", sum(is.na(base_final$estu_consecutivo)), "\n")
cat("  - Conteo de 'periodo' en NA:", sum(is.na(base_final$periodo)), "\n\n")

periodos_detectados <- sort(unique(na.omit(base_final$periodo)))
cat("Períodos válidos detectados en la serie:\n")
print(periodos_detectados)

resumen_periodos <- base_final[, .(registros = .N), by = periodo]
setorder(resumen_periodos, periodo)

cat("\nDistribución de registros por período:\n")
print(resumen_periodos)


# ============================================================
# 9. EXPORTACIÓN DEL ARCHIVO CONSOLIDADO
# ============================================================

# Usamos ruta_principal de forma portátil para que se guarde en Datos/
ruta_salida <- file.path(ruta_principal, "SABER 11 COMPLETO.csv")

cat("\nExportando base consolidada a disco...\n")
fwrite(
  base_final,
  file = ruta_salida,
  sep = ";",
  bom = TRUE,
  na = ""
)


# ============================================================
# 10. RESUMEN FINAL DEL PROCESO
# ============================================================

cat("\n============================================\n")
cat("PROCESO DE SABER 11 FINALIZADO CON ÉXITO\n")
cat("============================================\n")
cat("Archivo generado en:\n", ruta_salida, "\n")
cat("Total de registros finales:", nrow(base_final), "\n")
cat("Total de variables finales:", ncol(base_final), "\n")