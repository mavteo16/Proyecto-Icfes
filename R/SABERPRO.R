# ============================================================
# PROYECTO ICFES: CONSOLIDACIÓN Y ESTANDARIZACIÓN SABER PRO
# VENTANA TEMPORAL: 2014 - 2024 / 2025
# ============================================================
# Descripción: Este script automatiza la lectura, homologación,
# depuración y consolidación de los microdatos históricos de la 
# prueba Saber Pro (Competencias Genéricas) para la ventana 
# temporal establecida, asegurando un formato estandarizado.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y LIBRERÍAS
# ============================================================

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
  "2024" = file.path(ruta_principal, "Examen_Saber_Pro_Genericas_2024 (1).txt")
)


# ============================================================
# 3. SELECCIÓN DE VARIABLES OBJETIVO
# ============================================================

variables_objetivo <- c(
  "periodo",
  "estu_consecutivo",
  "estu_ano_examenestado_sb11",
  "estu_semestre_examenestadosb11",
  "estu_ano_egreso",
  "estu_ano_terminobachiller",
  "estu_coddane_cole_termino",
  "estu_codicfescole_termino",
  "estu_cole_termino",
  "estu_depto_reside",
  "estu_areareside",
  "estu_mcpio_reside",
  "estu_genero",
  "estu_discapacidad",
  "estu_etnia",
  "estu_inse_individual",
  "estu_nse_individual",
  "fami_estratovivienda",
  "fami_tieneinternet",
  "fami_educacionmadre",
  "fami_educacionpadre",
  "fami_ingreso_fmiliar_mensual",
  "fami_nivel_sisben",
  "estu_nivel_prgm_academico",
  "estu_metodo_prgm",
  "estu_prgm_academico",
  "estu_snies_prgmacademico",
  "estu_nucleo_pregrado",
  "estu_semestrecursa",
  "estu_porcentajecreditosaprob",
  "inst_cod_institucion",
  "inst_nombre_institucion",
  "inst_caracter_academico",
  "inst_origen",
  "estu_trabaja_actualmente",
  "estu_horassemanatrabaja",
  "estu_pagomatriculabeca",
  "estu_pagomatriculacredito",
  "estu_pagomatriculapadres",
  "estu_pagomatriculapropio",
  "estu_valormatriculauniversidad",
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
    cat("  -> UTF-8 falló. Reintentando lectura con codificación Latin-1...\n")
    
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
      stop(paste0("\nFallo crítico al leer el archivo:\n", ruta, "\n\nDetalle:\n", e$message))
    })
  }
  
  names(resultado) <- trimws(names(resultado))
  
  cat("  -> Lectura exitosa.\n")
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
      setnames(dt, old = nombres_originales[indices], new = variable)
    }
    
    if (length(indices) > 1) {
      warning(paste0("Múltiples columnas equivalentes a '", variable, "'. Se conservará la primera."))
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

base_consolidada <- rbindlist(
  lista_bases,
  use.names = TRUE,
  fill = TRUE
)

cat("Consolidación inicial completada.\n")
cat("Registros totales:", nrow(base_consolidada), "\n")
cat("Columnas totales:", ncol(base_consolidada), "\n")


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
cat("FILTRO TEMPORAL APLICADO\n")
cat("============================================\n")
cat("Registros antes del filtro:", filas_antes_filtro, "\n")
cat("Registros después del filtro:", filas_despues_filtro, "\n")
cat("Registros descartados fuera de rango:", filas_antes_filtro - filas_despues_filtro, "\n")


# ============================================================
# 9. ELIMINACIÓN DE COLUMNA AUXILIAR Y ORDENAMIENTO
# ============================================================

base_consolidada[, anio_periodo := NULL]

setcolorder(
  base_consolidada,
  variables_objetivo
)

base_final <- base_consolidada


# ============================================================
# 10. CONTROLES DE CALIDAD Y AUDITORÍA FINAL
# ============================================================

cat("\n============================================\n")
cat("REPORTE DE CONTROL DE CALIDAD - SABER PRO\n")
cat("============================================\n")

cat("Número total de filas:", nrow(base_final), "\n")
cat("Número total de columnas:", ncol(base_final), "\n")

faltantes_finales <- setdiff(variables_objetivo, names(base_final))

if (length(faltantes_finales) > 0) {
  stop(paste0("\nERROR CRÍTICO: Faltan variables en la base final:\n", paste(faltantes_finales, collapse = ", ")))
} else {
  cat("[OK] Verificación superada: Todas las variables de interés están presentes.\n")
}

cat("  - Conteo de valores NA en 'estu_consecutivo':", sum(is.na(base_final$estu_consecutivo)), "\n")
cat("  - Conteo de valores NA en 'periodo':", sum(is.na(base_final$periodo)), "\n")


# ============================================================
# 11. REVISIÓN DE PERÍODOS DETECTADOS
# ============================================================

cat("\n============================================\n")
cat("PERÍODOS Y COHORTES ENCONTRADAS\n")
cat("============================================\n")

periodos_encontrados <- sort(unique(na.omit(base_final$periodo)))
print(periodos_encontrados)


# ============================================================
# 12. DISTRIBUCIÓN DE REGISTROS POR AÑO
# ============================================================

base_final[, anio_tmp := suppressWarnings(as.integer(substr(periodo, 1, 4)))]

registros_por_anio <- base_final[, .(registros = .N), by = anio_tmp]
setorder(registros_por_anio, anio_tmp)

cat("\n============================================\n")
cat("REGISTROS POR AÑO (SABER PRO)\n")
cat("============================================\n")
print(registros_por_anio)

base_final[, anio_tmp := NULL]


# ============================================================
# 13. VISTA PREVIA DE LAS PRIMERAS FILAS
# ============================================================

cat("\n============================================\n")
cat("PRIMERAS FILAS DE LA BASE FINAL CONSOLIDADA\n")
cat("============================================\n")
print(head(base_final, 10))


# ============================================================
# 14. EXPORTACIÓN DEL ARCHIVO CONSOLIDADO
# ============================================================

# Usamos ruta_principal de forma portátil para que se guarde en Datos/
ruta_salida <- file.path(ruta_principal, "SABER PRO COMPLETO.csv")

cat("\nExportando base consolidada a disco...\n")
fwrite(
  base_final,
  file = ruta_salida,
  sep = ";",
  bom = TRUE,
  na = ""
)


# ============================================================
# 15. CONFIRMACIÓN FINAL DEL PROCESO
# ============================================================

cat("\n============================================\n")
cat("PROCESO DE SABER PRO FINALIZADO CORRECTAMENTE\n")
cat("============================================\n")
cat("Archivo guardado en:\n", ruta_salida, "\n\n")
cat("Registros finales consolidados:", nrow(base_final), "\n")
cat("Variables finales conservadas:", ncol(base_final), "\n")