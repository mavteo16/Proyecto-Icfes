# ============================================================
# PROYECTO ICFES: HOMOLOGACIÓN DE VARIABLES - SABER 11 (2014-1)
# ============================================================
# Descripción: Este script realiza la lectura específica de la cohorte
# 2014-1, verifica la existencia de sus campos particulares, renombra 
# las columnas desfasadas (como lenguaje o sociales) para alinearlas 
# con el estándar general, y exporta la base homologada lista para la unión.
# ============================================================


# ============================================================
# 1. CARGA DE LIBRERÍAS
# ============================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

library(data.table)


# ============================================================
# 2. DEFINICIÓN DE RUTA DEL ARCHIVO FUENTE
# ============================================================

ruta_20141 <- file.path(ruta_principal, "Examen_Saber_11_20141.txt")


# ============================================================
# 3. LECTURA DE LA BASE DE DATOS
# ============================================================

cat("\n============================================\n")
cat("LEYENDO BASE 2014-1...\n")
cat("============================================\n")

base_20141 <- fread(
  file = ruta_20141,
  sep = ";",
  encoding = "UTF-8",
  quote = "\"",
  fill = Inf,
  header = TRUE,
  check.names = FALSE,
  strip.white = TRUE,
  data.table = TRUE
)

cat("Lectura exitosa. Filas:", nrow(base_20141), "| Columnas:", ncol(base_20141), "\n")


# ============================================================
# 4. DEFINICIÓN DE VARIABLES REQUERIDAS (INICIALES)
# ============================================================

variables_20141 <- c(
  "periodo",
  "estu_consecutivo",
  "estu_estudiante",
  "cole_area_ubicacion",
  "cole_bilingue",
  "cole_calendario",
  "cole_jornada",
  "cole_naturaleza",
  "estu_depto_reside",
  "fami_estratovivienda",
  "fami_tieneinternet",
  "punt_ingles",
  "punt_lenguaje",
  "punt_matematicas",
  "punt_ciencias_sociales",
  "recaf_punt_c_naturales"
)


# ============================================================
# 5. VERIFICACIÓN PREVIA DE EXISTENCIA
# ============================================================

faltantes <- setdiff(
  variables_20141,
  names(base_20141)
)

if (length(faltantes) > 0) {
  cat("\n============================================\n")
  cat("ERROR: VARIABLES NO ENCONTRADAS EN 2014-1\n")
  cat("============================================\n\n")
  print(faltantes)
  stop("\nEl proceso se detuvo porque faltan variables indispensables.")
}

cat("[OK] Verificación superada: Todas las variables esperadas están presentes.\n")


# ============================================================
# 6. HOMOLOGACIÓN DE NOMBRES DE COLUMNAS
# ============================================================
# Se renombran las variables antiguas a los nombres estándar
# utilizados en el resto de la serie temporal (2014-2025).
# ============================================================

setnames(
  base_20141,
  old = c(
    "punt_lenguaje",
    "punt_ciencias_sociales",
    "recaf_punt_c_naturales"
  ),
  new = c(
    "punt_lectura_critica",
    "punt_sociales_ciudadanas",
    "punt_c_naturales"
  )
)


# ============================================================
# 7. DEFINICIÓN DEL VECTOR DE VARIABLES FINALES
# ============================================================

variables_finales <- c(
  "periodo",
  "estu_consecutivo",
  "estu_estudiante",
  "cole_area_ubicacion",
  "cole_bilingue",
  "cole_calendario",
  "cole_jornada",
  "cole_naturaleza",
  "estu_depto_reside",
  "fami_estratovivienda",
  "fami_tieneinternet",
  "punt_c_naturales",
  "punt_ingles",
  "punt_lectura_critica",
  "punt_matematicas",
  "punt_sociales_ciudadanas"
)


# ============================================================
# 8. SUBSECCIÓN: CONSERVAR SOLO LAS VARIABLES HOMOLOGADAS
# ============================================================

base_20141_homologada <- base_20141[
  ,
  ..variables_finales
]


# ============================================================
# 9. VERIFICACIÓN Y AUDITORÍA FINAL
# ============================================================

faltantes_finales <- setdiff(
  variables_finales,
  names(base_20141_homologada)
)

if (length(faltantes_finales) > 0) {
  stop(
    paste(
      "ERROR CRÍTICO: Faltan variables después de la homologación:",
      paste(faltantes_finales, collapse = ", ")
    )
  )
}

cat("\n============================================\n")
cat("HOMOLOGACIÓN EXITOSA - COHORTE 2014-1\n")
cat("============================================\n")
cat("  - Filas procesadas:", nrow(base_20141_homologada), "\n")
cat("  - Variables finales:", ncol(base_20141_homologada), "\n\n")

cat("Columnas resultantes:\n")
print(names(base_20141_homologada))


# ============================================================
# 10. EXPORTACIÓN DE LA BASE HOMOLOGADA
# ============================================================

ruta_salida <- file.path(ruta_principal, "Saber11_20141_homologada.csv")

cat("\nGuardando archivo homologado en disco...\n")
fwrite(
  base_20141_homologada,
  file = ruta_salida,
  sep = ";",
  bom = TRUE,
  na = ""
)

cat("\n============================================\n")
cat("ARCHIVO GUARDADO CORRECTAMENTE\n")
cat("============================================\n")
cat(ruta_salida, "\n")