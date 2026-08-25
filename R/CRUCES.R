# ============================================================
# PROYECTO ICFES: FILTRADO, CÁLCULO DE REZAGO Y CONSOLIDACIÓN DE CRUCES
# VENTANA TEMPORAL: 2014 - 2025 (TODOS LOS PERÍODOS)
# ============================================================
# Descripción: Este script lee el archivo bruto de cruces entre pruebas,
# valida las variables requeridas, filtra la ventana temporal permitida
# para Saber 11 y Saber Pro, calcula el rezago académico en semestres,
# aplica la regla de consistencia temporal (Saber Pro posterior a Saber 11)
# y exporta la base de emparejamientos válidos definitiva.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y LIBRERÍAS
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

library(data.table)


# ============================================================
# 2. DEFINICIÓN DE RUTAS DE LOS ARCHIVOS FUENTE
# ============================================================

rutas_archivos <- c(
  "cruce_1" = file.path(ruta_principal, "Cruce Examen Saber 11 - Examen Saber Pro (3).txt")
)


# ============================================================
# 3. DEFINICIÓN DE VARIABLES REQUERIDAS PARA EL CRUCE
# ============================================================

variables_requeridas <- c(
  "estu_consecutivo_sb11",
  "periodo_sb11",
  "estu_consecutivo_sbpro",
  "periodo_sbpro"
)


# ============================================================
# 4. LECTURA Y VALIDACIÓN DE LA(S) BASE(S) DE CRUCE
# ============================================================

lista_bases <- list()

for (nombre_archivo in names(rutas_archivos)) {
  ruta <- rutas_archivos[[nombre_archivo]]
  
  cat("\n============================================\n")
  cat("LEYENDO ARCHIVO DE CRUCE:", nombre_archivo, "\n")
  cat("Ruta:", ruta, "\n")
  cat("============================================\n")
  
  # A. Lectura de los datos
  datos <- fread(
    file = ruta,
    sep = "auto",
    encoding = "UTF-8",
    header = TRUE,
    check.names = FALSE,
    strip.white = TRUE
  )
  
  # B. Limpieza de espacios en los nombres de columnas
  names(datos) <- trimws(names(datos))
  
  # C. Verificación de integridad de variables obligatorias
  faltantes <- setdiff(variables_requeridas, names(datos))
  
  if (length(faltantes) > 0) {
    stop(paste0(
      "\nERROR CRÍTICO en ", nombre_archivo, 
      "\nFaltan las siguientes variables indispensables:\n", 
      paste(faltantes, collapse = ", ")
    ))
  }
  
  # D. Conservación exclusiva de las 4 variables esenciales del cruce
  datos <- datos[, .(
    estu_consecutivo_sb11,
    periodo_sb11,
    estu_consecutivo_sbpro,
    periodo_sbpro
  )]
  
  # E. Almacenamiento en lista temporal
  lista_bases[[nombre_archivo]] <- datos
}


# ============================================================
# 5. UNIFICACIÓN DE LAS BASES DE CRUCE
# ============================================================

cat("\n============================================\n")
cat("UNIFICANDO BASES DE CRUCE...\n")
cat("============================================\n")

base_cruces <- rbindlist(
  lista_bases,
  use.names = TRUE,
  fill = FALSE
)

cat("Registros iniciales unificados:", nrow(base_cruces), "\n")


# ============================================================
# 6. LIMPIEZA DE CAMPOS DE PERÍODO
# ============================================================

base_cruces[, periodo_sb11 := trimws(as.character(periodo_sb11))]
base_cruces[, periodo_sbpro := trimws(as.character(periodo_sbpro))]


# ============================================================
# 7. CONVERSIÓN NUMÉrica Y EXTRACCIÓN DE AÑOS
# ============================================================
# Nota: Se convierten a entero y se usa división entera por 10 
# (ej: 20141 %/% 10 = 2014) para aislar el año de aplicación.
# ============================================================

base_cruces[, periodo_sb11_num := suppressWarnings(as.integer(periodo_sb11))]
base_cruces[, periodo_sbpro_num := suppressWarnings(as.integer(periodo_sbpro))]

base_cruces[, anio_sb11 := periodo_sb11_num %/% 10]
base_cruces[, anio_sbpro := periodo_sbpro_num %/% 10]


# ============================================================
# 8. FILTRADO POR RANGO TEMPORAL (2014 - 2025)
# ============================================================

# 8.1. Filtro para Saber 11
base_cruces <- base_cruces[
  anio_sb11 >= 2014 &
    anio_sb11 <= 2025
]

# 8.2. Filtro para Saber Pro
base_cruces <- base_cruces[
  anio_sbpro >= 2014 &
    anio_sbpro <= 2025
]


# ============================================================
# 9. CÁLCULO DEL REZAGO ACADÉMICO ENTRE PRUEBAS
# ============================================================
# Fórmula que transforma los períodos en semestres y calcula la diferencia.
# ============================================================

base_cruces[, rezago_semestres := (
  ((periodo_sbpro_num %/% 10) - (periodo_sb11_num %/% 10)) * 2
) + (
  ((periodo_sbpro_num %% 10) - (periodo_sb11_num %% 10))
)]


# ============================================================
# 10. APLICACIÓN DE REGLA DE CONSISTENCIA TEMPORAL (REZAGO > 0)
# ============================================================
# Exigencia metodológica: El Saber Pro debe ser estrictamente posterior
# al Saber 11. Se descartan órdenes invertidos o emparejamientos simultáneos.
# ============================================================

base_cruces <- base_cruces[
  rezago_semestres > 0
]


# ============================================================
# 11. ELIMINACIÓN DE DUPLICADOS Y SELECCIÓN DE COLUMNAS
# ============================================================

base_cruces <- unique(
  base_cruces[, .(
    estu_consecutivo_sb11,
    periodo_sb11,
    estu_consecutivo_sbpro,
    periodo_sbpro,
    rezago_semestres
  )]
)


# ============================================================
# 12. ORDENAMIENTO CRONOLÓGICO DE LA BASE
# ============================================================

setorder(
  base_cruces,
  periodo_sb11,
  periodo_sbpro
)

base_final <- base_cruces[, .(
  estu_consecutivo_sb11,
  periodo_sb11,
  estu_consecutivo_sbpro,
  periodo_sbpro,
  rezago_semestres
)]


# ============================================================
# 13. REPORTE Y CONTROLES DE CALIDAD
# ============================================================

cat("\n============================================\n")
cat("RESULTADO FINAL DEL CRUCE Y FILTRADO\n")
cat("============================================\n")

cat("Total de emparejamientos válidos (rezago > 0):", nrow(base_final), "\n")
cat("Estudiantes Saber 11 únicos en el panel:", uniqueN(base_final$estu_consecutivo_sb11), "\n")
cat("Estudiantes Saber Pro únicos en el panel:", uniqueN(base_final$estu_consecutivo_sbpro), "\n\n")

cat("Vista previa de los primeros 10 registros:\n")
print(head(base_final, 10))


# ============================================================
# 14. EXPORTACIÓN DE LA BASE DE CRUCES DEPURADA
# ============================================================

ruta_salida <- file.path(ruta_principal, "CRUCE_SABER11_SABERPRO_2014_2025.csv")

cat("\nExportando base de cruces depurada a disco...\n")
fwrite(
  base_final,
  ruta_salida,
  sep = ";",
  bom = TRUE
)

cat("\n============================================\n")
cat("PROCESO FINALIZADO CON ÉXITO\n")
cat("============================================\n")
cat("Base final guardada en:\n", ruta_salida, "\n")
# Al final de CRUCES.R:
ruta_salida_cruce <- file.path(ruta_principal, "CRUCE_SABER11_SABERPRO_2014_2025.csv")
fwrite(base_final, ruta_salida_cruce, sep = ";", bom = TRUE)
