# ============================================================
# SCRIPT DE DETECCIÓN, CONTEO Y ELIMINACIÓN DE ANOMALÍAS
# ============================================================
# Descripción: Inspecciona los puntajes de materias de Saber Pro,
# detecta anomalías (>300 o decimales), cuenta estudiantes afectados,
# elimina a dichos estudiantes y exporta la base de datos depurada.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
})

cat("============================================================\n")
cat("     DEPURACIÓN Y ELIMINACIÓN DE ANOMALÍAS EN SABER PRO     \n")
cat("============================================================\n\n")

# 1. Cargar la base de datos consolidada
ruta_base <- "Resultados_Directos_S11_SPro/Base_Consolidada.csv"

if (!file.exists(ruta_base)) {
  stop(sprintf("No se encontró el archivo en: %s. Asegúrate de correr los pasos anteriores.", ruta_base))
}

cat("Leyendo la base de datos...\n")
df <- fread(file = ruta_base, colClasses = "character", data.table = FALSE)
total_filas_inicial <- nrow(df)

# 2. Identificar las variables de puntajes de materias de Saber Pro
vars_candidatas <- grep("^SP_.*_punt$", colnames(df), value = TRUE)
vars_materias_sp <- vars_candidatas[!grepl("global|percentil", vars_candidatas)]

cat(sprintf("-> Variables de materias Saber Pro detectadas: %d\n", length(vars_materias_sp)))

# Determinar columna de período para Saber Pro
col_periodo <- "periodo_sbpro"
if (!col_periodo %in% colnames(df)) {
  col_periodo <- "periodo"
}

# Identificar ID del estudiante para conteo único
col_id <- intersect(c("estu_consecutivo_sbpro", "estu_consecutivo", "estu_consecutivo_sb11"), colnames(df))
if(length(col_id) == 0) {
  df$id_temp <- 1:nrow(df)
  col_id <- "id_temp"
} else {
  col_id <- col_id[1]
}

# 3. Construir matriz de análisis para detectar anomalías por celda y por estudiante
matriz_anomalias <- data.frame(
  Estudiante_ID = df[[col_id]],
  Periodo = df[[col_periodo]]
)

resultados_variables <- data.frame()

for (v in vars_materias_sp) {
  val_txt <- trimws(df[[v]])
  val_num <- suppressWarnings(as.numeric(gsub(",", ".", val_txt)))
  
  es_decimal <- (!is.na(val_num) & (val_num %% 1 != 0)) | grepl("\\.[1-9]|,[1-9]", val_txt)
  es_mayor_300 <- !is.na(val_num) & val_num > 300
  es_anomalia <- es_decimal | es_mayor_300
  
  matriz_anomalias[[v]] <- es_anomalia
  
  temp_df <- data.frame(
    Periodo = df[[col_periodo]],
    Valor_Texto = val_txt,
    Es_Anomalia = es_anomalia
  )
  
  anomalias_v <- temp_df %>% filter(Es_Anomalia & !is.na(Periodo) & Periodo != "")
  
  if (nrow(anomalias_v) > 0) {
    resumen_v <- anomalias_v %>%
      group_by(Periodo) %>%
      summarise(
        Variable = v,
        Cantidad_Anomalias = n(),
        Ejemplos_Valores = paste(head(unique(Valor_Texto), 3), collapse = ", ")
      )
    resultados_variables <- rbind(resultados_variables, resumen_v)
  }
}

matriz_anomalias$Tiene_Anomalia_General <- rowSums(matriz_anomalias[, vars_materias_sp], na.rm = TRUE) > 0

# 4. Reportes en consola
cat("============================================================\n")
cat("      1. REPORTE DE ANOMALÍAS POR VARIABLE Y PERÍODO        \n")
cat("============================================================\n")

if (nrow(resultados_variables) > 0) {
  resultados_variables <- resultados_variables %>% arrange(Periodo, Variable)
  print(as.data.frame(resultados_variables), row.names = FALSE)
} else {
  cat("[OK] No se encontraron anomalías por variable.\n")
}

cat("\n============================================================\n")
cat("     2. ESTUDIANTES CON AL MENOS UNA ANOMALÍA POR PERÍODO   \n")
cat("============================================================\n")

estudiantes_por_periodo <- matriz_anomalias %>%
  filter(!is.na(Periodo) & Periodo != "" & Tiene_Anomalia_General) %>%
  group_by(Periodo) %>%
  summarise(Estudiantes_Afectados = n_distinct(Estudiante_ID)) %>%
  arrange(Periodo)

if (nrow(estudiantes_por_periodo) > 0) {
  print(as.data.frame(estudiantes_por_periodo), row.names = FALSE)
} else {
  cat("[OK] Ningún estudiante presenta anomalías.\n")
}

cat("\n============================================================\n")
cat("                 3. RESUMEN GLOBAL TOTAL                    \n")
cat("============================================================\n")

total_celdas_anomalas <- sum(resultados_variables$Cantidad_Anomalias, na.rm = TRUE)
total_estudiantes_global <- matriz_anomalias %>%
  filter(Tiene_Anomalia_General & !is.na(Periodo) & Periodo != "") %>%
  summarise(Total = n_distinct(Estudiante_ID)) %>%
  pull(Total)

cat(sprintf("  -> Total global de celdas con anomalías: %s\n", format(total_celdas_anomalas, big.mark = ",")))
cat(sprintf("  -> Total global de estudiantes afectados: %s\n", format(total_estudiantes_global, big.mark = ",")))

# 5. Eliminación de estudiantes anómalos y exportación de la nueva base limpia
cat("\n============================================================\n")
cat("          4. DEPURACIÓN Y EXPORTACIÓN DE LA BASE            \n")
cat("============================================================\n")

df_depurada <- df[!matriz_anomalias$Tiene_Anomalia_General, ]
total_filas_final <- nrow(df_depurada)
filas_eliminadas <- total_filas_inicial - total_filas_final

ruta_salida <- "Resultados_Directos_S11_SPro/Base_Consolidada_Sin_Anomalias.csv"
fwrite(df_depurada, ruta_salida)

cat(sprintf("  -> Registros iniciales en la base: %s\n", format(total_filas_inicial, big.mark = ",")))
cat(sprintf("  -> Registros eliminados (estudiantes con anomalías): %s\n", format(filas_eliminadas, big.mark = ",")))
cat(sprintf("  -> Registros finales en la base depurada: %s\n", format(total_filas_final, big.mark = ",")))
cat(sprintf("\n[¡ÉXITO!] Base depurada guardada en: %s\n", ruta_salida))
cat("============================================================\n")