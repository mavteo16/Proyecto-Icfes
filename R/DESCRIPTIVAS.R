# ============================================================
# PROYECTO ICFES: PERFILAMIENTO Y ESTADÍSTICA DESCRIPTIVA DETALLADA
# ============================================================
# Descripción: Este script carga la base de datos limpia y validada,
# procesa exclusivamente las 17 variables cuantitativas clave (puntajes, 
# índices y rezago) y las variables cualitativas (socioeconómicas 
# y de contexto, omitiendo estu_repite e IDs), y exporta un informe ejecutivo 
# estructurado en Excel con múltiples pestañas profesionales.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y CARGA DE LIBRERÍAS
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(openxlsx)
})

if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}

directorio_salida <- "Resultados_Directos_S11_SPro"

cat("============================================================\n")
cat("   INICIANDO MÓDULO DE ESTADÍSTICA DESCRIPTIVA AVANZADA     \n")
cat("============================================================\n\n")


# ============================================================
# 2. CARGA DE LA BASE DE DATOS LIMPIA
# ============================================================

ruta_base <- file.path(directorio_salida, "Base_Consolidada_Limpia.csv")

if (!file.exists(ruta_base)) {
  stop(sprintf("No se encontró el archivo limpio en: %s. Asegúrate de ejecutar los pasos previos.", ruta_base))
}

cat("Leyendo la base de datos consolidada limpia...\n")
df <- fread(ruta_base, colClasses = "character") %>% as.data.frame()
total_filas <- nrow(df)

cat(sprintf("  -> Total de registros analizados: %s\n", format(total_filas, big.mark = ",")))
cat(sprintf("  -> Total de columnas en la base: %s\n\n", ncol(df)))


# ============================================================
# 3. DEFINICIÓN EXPLÍCITA DE LISTAS OBJETIVO (SIN IDS NI ESTU_REPITE)
# ============================================================

# A. Las 17 variables cuantitativas estrictas (con INSE incluido)
vars_cuant_objetivo <- c(
  "S11_punt_c_naturales", "S11_punt_ingles", "S11_punt_lectura_critica", 
  "S11_punt_matematicas", "S11_punt_sociales_ciudadanas", "S11_punt_global", 
  "S11_percentil_global", "S11_estu_inse_individual", "SP_estu_nse_individual", 
  "SP_mod_comuni_escrita_punt", "SP_mod_competen_ciudada_punt", "SP_mod_ingles_punt", 
  "SP_mod_lectura_critica_punt", "SP_mod_razona_cuantitat_punt", "SP_punt_global", 
  "SP_percentil_global", "rezago_semestres"
)

# B. Las variables cualitativas / categóricas (sin estu_repite)
vars_cual_objetivo <- c(
  "periodo_sb11", "periodo_sbpro", "S11_estu_estudiante", "S11_estu_depto_reside", 
  "S11_fami_estratovivienda", "S11_fami_tieneinternet", 
  "S11_cole_naturaleza", "S11_cole_area_ubicacion", "S11_cole_bilingue", 
  "S11_cole_calendario", "S11_cole_jornada", "SP_estu_genero", "SP_estu_depto_reside", 
  "SP_fami_educacionmadre", "SP_fami_educacionpadre", "SP_fami_ingreso_fmiliar_mensual", 
  "SP_estu_trabaja_actualmente", "SP_inst_nombre_institucion", "SP_estu_prgm_academico"
)

# Asegurar existencia real en el dataframe
vars_cuant <- intersect(vars_cuant_objetivo, colnames(df))
vars_cual  <- intersect(vars_cual_objetivo, colnames(df))

cat(sprintf("-> Variables cuantitativas identificadas para cálculo: %d\n", length(vars_cuant)))
cat(sprintf("-> Variables cualitativas identificadas para frecuencia: %d\n\n", length(vars_cual)))


# ============================================================
# 4. CÁLCULO DE ESTADÍSTICOS PARA VARIABLES CUANTITATIVAS
# ============================================================

cat("Calculando estadísticos robustos para variables cuantitativas...\n")

lista_stats_cuant <- lapply(vars_cuant, function(v) {
  val <- suppressWarnings(as.numeric(df[[v]]))
  val_validos <- val[!is.na(val)]
  n_val <- length(val_validos)
  n_na  <- sum(is.na(val))
  
  if (n_val == 0) {
    return(data.frame(
      Variable = v, N_Validos = 0, N_Perdidos_NA = n_na, 
      Media = NA, Mediana = NA, Desv_Estandar = NA, 
      IQR = NA, P25 = NA, P75 = NA, P95 = NA, Minimo = NA, Maximo = NA
    ))
  }
  
  data.frame(
    Variable      = v,
    N_Validos     = n_val,
    N_Perdidos_NA = n_na,
    Media         = round(mean(val_validos, na.rm = TRUE), 2),
    Mediana       = round(median(val_validos, na.rm = TRUE), 2),
    Desv_Estandar = round(sd(val_validos, na.rm = TRUE), 2),
    IQR           = round(IQR(val_validos, na.rm = TRUE), 2),
    P25           = round(quantile(val_validos, 0.25, na.rm = TRUE), 2),
    P75           = round(quantile(val_validos, 0.75, na.rm = TRUE), 2),
    P95           = round(quantile(val_validos, 0.95, na.rm = TRUE), 2),
    Minimo        = min(val_validos, na.rm = TRUE),
    Maximo        = max(val_validos, na.rm = TRUE)
  )
})

tabla_cuantitativas <- bind_rows(lista_stats_cuant)


# ============================================================
# 5. CÁLCULO DE FRECUENCIAS PARA VARIABLES CUALITATIVAS
# ============================================================

cat("Calculando distribuciones de frecuencia y porcentajes para cualitativas...\n")

lista_frecuencias <- lapply(vars_cual, function(v) {
  x <- df[[v]]
  x[is.na(x) | x == ""] <- "NO REPORTADO / NA"
  
  freq_df <- as.data.frame(table(x, useNA = "no"))
  colnames(freq_df) <- c("Categoria", "Frecuencia_Absoluta")
  
  freq_df <- freq_df %>%
    mutate(
      Variable            = v,
      Porcentaje          = round((Frecuencia_Absoluta / total_filas) * 100, 2),
      Porcentaje_Validos  = round((Frecuencia_Absoluta / sum(Frecuencia_Absoluta[Categoria != "NO REPORTADO / NA"])) * 100, 2)
    ) %>%
    select(Variable, Categoria, Frecuencia_Absoluta, Porcentaje, Porcentaje_Validos) %>%
    arrange(desc(Frecuencia_Absoluta))
  
  return(freq_df)
})

tabla_cualitativas <- bind_rows(lista_frecuencias)


# ============================================================
# 6. TENDENCIA LONGITUDINAL (POR COHORTE / PERÍODO DE SABER 11)
# ============================================================

cat("Generando perfil de tendencia longitudinal por cohorte...\n")

col_periodo_s11 <- intersect(c("periodo_sb11", "s11_periodo", "periodo"), colnames(df))
if (length(col_periodo_s11) > 0) {
  col_p <- col_periodo_s11[1]
  
  tendencia_s11 <- df %>%
    mutate(punt_glob_num = suppressWarnings(as.numeric(S11_punt_global))) %>%
    group_by(Periodo_S11 = .data[[col_p]]) %>%
    summarise(
      Total_Estudiantes = n(),
      Media_Punt_Global = round(mean(punt_glob_num, na.rm = TRUE), 2),
      Mediana_Punt_Global = round(median(punt_glob_num, na.rm = TRUE), 2),
      Desv_Est_Punt_Global = round(sd(punt_glob_num, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(!is.na(Periodo_S11) & Periodo_S11 != "") %>%
    arrange(Periodo_S11)
} else {
  tendencia_s11 <- data.frame(Mensaje = "No se encontró la columna de período de Saber 11.")
}


# ============================================================
# 7. EXPORTACIÓN A EXCEL MULTIPESTAÑA PROFESIONAL
# ============================================================

cat("\n============================================\n")
cat("EXPORTANDO REPORTE ESTADÍSTICO A EXCEL...\n")
cat("============================================\n")

ruta_excel_stats <- file.path(directorio_salida, "Estadistica_Descriptiva_Detallada.xlsx")

wb <- createWorkbook()

# Pestaña 1: Cuantitativas (Puntajes, Percentiles, Rezago)
addWorksheet(wb, "Puntajes_Y_Cuantitativas")
writeData(wb, "Puntajes_Y_Cuantitativas", tabla_cuantitativas)

# Pestaña 2: Cualitativas (Frecuencias absolutas y relativas)
addWorksheet(wb, "Variables_Cualitativas")
writeData(wb, "Variables_Cualitativas", tabla_cualitativas)

# Pestaña 3: Tendencia Longitudinal por Cohorte
addWorksheet(wb, "Tendencia_Longitudinal_S11")
writeData(wb, "Tendencia_Longitudinal_S11", tendencia_s11)

saveWorkbook(wb, ruta_excel_stats, overwrite = TRUE)

cat(sprintf("[¡ÉXITO!] Reporte estadístico avanzado guardado en:\n  -> %s\n", ruta_excel_stats))
cat("============================================================\n")