# ============================================================
# PROYECTO ICFES: FILTRADO, CÁLCULO DE REZAGO Y CONSOLIDACIÓN DE CRUCES
# VENTANA TEMPORAL: SABER 11 (2010-2025) | SABER PRO (2014-2025)
# ============================================================
# Descripción: Este script lee el archivo bruto de cruces entre pruebas,
# valida las variables requeridas, aplica los filtros temporales diferenciados,
# calcula el rezago académico, audita matemáticamente las reglas y exporta la base.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y LIBRERÍAS
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

library(data.table)

# Seguridad por si se ejecuta de forma independiente
if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}


# ============================================================
# 2. DEFINICIÓN DE RUTAS DE LOS ARCHIVOS FUENTE
# ============================================================

rutas_archivos <- c(
  "cruce_1" = file.path(ruta_principal, "Cruce Examen Saber 11 - Examen Saber Pro.txt")
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
  
  datos <- fread(
    file = ruta,
    sep = "auto",
    encoding = "UTF-8",
    header = TRUE,
    check.names = FALSE,
    strip.white = TRUE
  )
  
  names(datos) <- trimws(names(datos))
  
  faltantes <- setdiff(variables_requeridas, names(datos))
  
  if (length(faltantes) > 0) {
    stop(paste0(
      "\nERROR CRÍTICO en ", nombre_archivo, 
      "\nFaltan las siguientes variables indispensables:\n", 
      paste(faltantes, collapse = ", ")
    ))
  }
  
  datos <- datos[, .(
    estu_consecutivo_sb11,
    periodo_sb11,
    estu_consecutivo_sbpro,
    periodo_sbpro
  )]
  
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
# 7. CONVERSIÓN NUMÉRICA Y EXTRACCIÓN DE AÑOS
# ============================================================

base_cruces[, periodo_sb11_num := suppressWarnings(as.integer(periodo_sb11))]
base_cruces[, periodo_sbpro_num := suppressWarnings(as.integer(periodo_sbpro))]

base_cruces[, anio_sb11 := periodo_sb11_num %/% 10]
base_cruces[, anio_sbpro := periodo_sbpro_num %/% 10]


# ============================================================
# 8. FILTRADO POR RANGO TEMPORAL DIFERENCIADO
# ============================================================

# 8.1. Filtro para Saber 11 (2010 a 2025)
base_cruces <- base_cruces[
  anio_sb11 >= 2010 &
    anio_sb11 <= 2025
]

# 8.2. Filtro para Saber Pro (2014 a 2025)
base_cruces <- base_cruces[
  anio_sbpro >= 2014 &
    anio_sbpro <= 2025
]


# ============================================================
# 9. CÁLCULO DEL REZAGO ACADÉMICO ENTRE PRUEBAS
# ============================================================

base_cruces[, rezago_semestres := (
  ((periodo_sbpro_num %/% 10) - (periodo_sb11_num %/% 10)) * 2
) + (
  ((periodo_sbpro_num %% 10) - (periodo_sb11_num %% 10))
)]


# ============================================================
# 10. APLICACIÓN DE REGLA DE CONSISTENCIA TEMPORAL (REZAGO > 0)
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
# 13. AUDITORÍA NUMÉRICA Y CONTROLES DE CALIDAD
# ============================================================

cat("\n============================================\n")
cat("RESULTADO FINAL DEL CRUCE Y AUDITORÍA NUMÉRICA\n")
cat("============================================\n")

# Auditorías matemáticas cuantitativas
min_s11  <- min(as.integer(substr(base_final$periodo_sb11, 1, 4)), na.rm = TRUE)
min_spro <- min(as.integer(substr(base_final$periodo_sbpro, 1, 4)), na.rm = TRUE)
errores_rezago <- sum(base_final$rezago_semestres <= 0, na.rm = TRUE)

cat(sprintf("  - Año mínimo Saber 11 en cruce: %d (Criterio >= 2010) -> %s\n", min_s11, if(min_s11 >= 2010) "[OK]" else "[ERROR]"))
cat(sprintf("  - Año mínimo Saber Pro en cruce: %d (Criterio >= 2014) -> %s\n", min_spro, if(min_spro >= 2014) "[OK]" else "[ERROR]"))
cat(sprintf("  - Registros con rezago inválido (<= 0): %d (Criterio == 0) -> %s\n\n", errores_rezago, if(errores_rezago == 0) "[OK]" else "[ERROR]"))

cat("Total de emparejamientos válidos (rezago > 0):", nrow(base_final), "\n")
cat("Estudiantes Saber 11 únicos en el panel:", uniqueN(base_final$estu_consecutivo_sb11), "\n")
cat("Estudiantes Saber Pro únicos en el panel:", uniqueN(base_final$estu_consecutivo_sbpro), "\n\n")

cat("Distribución cuantitativa por semestres de rezago:\n")
print(base_final[, .(Emparejamientos = .N), by = rezago_semestres][order(rezago_semestres)])


# ============================================================
# 14. EXPORTACIÓN DE LA BASE DE CRUCES DEPURADA
# ============================================================

ruta_salida <- file.path(ruta_principal, "CRUCE_SABER11_SABERPRO_2010_2025.csv")

cat("\nExportando base de cruces depurada a disco...\n")
fwrite(
  base_final,
  ruta_salida,
  sep = ";",
  bom = TRUE
)

cat("\n============================================\n")
cat("PROCESO DE CRUCES FINALIZADO CON ÉXITO\n")
cat("============================================\n")
cat("Base final guardada en:\n", ruta_salida, "\n")