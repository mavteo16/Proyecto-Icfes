# ============================================================
# PROYECTO ICFES: LIMPIEZA, UNIFICACIÓN Y VALIDACIÓN DE VARIABLES
# ============================================================
# Descripción: Este script carga la base de datos depurada (sin anomalías), 
# aplica reglas estrictas de estandarización y limpieza de texto campo por campo,
# ejecuta una comprobación matemática de frecuencias y exporta la base limpia
# junto a su reporte de validación en CSV y Excel.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y CARGA DE LIBRERÍAS
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(openxlsx)
})

if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}


# ============================================================
# 2. CARGA DE LA BASE DE DATOS SIN ANOMALÍAS (PASO PREVIO)
# ============================================================

ruta_base <- "Resultados_Directos_S11_SPro/Base_Consolidada_Sin_Anomalias.csv"

cat("\n============================================\n")
cat("1. LEYENDO LA BASE DE DATOS DEPURADA...\n")
cat("============================================\n")

if (!file.exists(ruta_base)) {
  stop(paste0("No se encontró el archivo de entrada en: ", ruta_base, ". Asegúrate de que el Paso 6 haya corrido bien."))
}

df <- fread(ruta_base, colClasses = "character") %>% as.data.frame()
total_filas_inicial <- nrow(df)

cat(sprintf("  -> Registros totales en la base depurada: %s\n\n", format(total_filas_inicial, big.mark = ",")))


# ============================================================
# 3. FUNCIÓN AUXILIAR PARA LIMPIAR NULOS Y VALORES BASURA
# ============================================================

limpiar_nulos <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x %in% c("", "[VACÍO O NA]", "NA", "null", "NULL", "N/A", ".")] <- NA
  x <- trimws(x)
  x[x == "" | grepl("^\"+$", x)] <- NA
  return(x)
}


# ============================================================
# 4. APLICACIÓN DE CRITERIOS DE LIMPIEZA VARIABLE POR VARIABLE
# ============================================================

cat("2. Aplicando reglas de unificación y limpieza...\n")

# ------------------------------------------------------------
# 4.1. Limpieza de Departamentos (Saber 11 y Saber Pro)
# ------------------------------------------------------------
limpiar_depto_vector <- function(x, es_saber_pro = FALSE) {
  x <- limpiar_nulos(x)
  if(all(is.na(x))) return(x)
  
  ciudades_extranjeras <- c(
    "ROMA", "MADRID", "NUEVA YORK", "ATLANTA", "BARCELONA", 
    "CIUDAD DE PANAMÁ", "FLANDES ORIENTAL", "FRANKFURT", 
    "GUADALAJARA", "HOUSTON", "LEIRIA", "LOS ANGELES", 
    "MIAMI", "PARIS", "QUEBEC", "RICHMOND", "SACRAMENTO", 
    "SYDNEY", "VALENCIA"
  )
  
  if(es_saber_pro) {
    x[toupper(x) %in% ciudades_extranjeras] <- "EXTRANJERO"
  }
  
  case_when(
    toupper(x) %in% c("BOGOTA", "BOGOTÁ", "BOGOTÁ D.C.") ~ "Bogotá",
    toupper(x) == "ANTIOQUIA" ~ "Antioquia",
    toupper(x) == "VALLE" ~ "Valle del Cauca",
    toupper(x) == "CUNDINAMARCA" ~ "Cundinamarca",
    toupper(x) == "ATLANTICO" ~ "Atlántico",
    toupper(x) == "SANTANDER" ~ "Santander",
    toupper(x) == "BOLIVAR" ~ "Bolívar",
    toupper(x) == "BOYACA" ~ "Boyacá",
    toupper(x) %in% c("NORTE SANTANDER", "NORTE DE SANTANDER") ~ "Norte de Santander",
    toupper(x) == "NARIÑO" ~ "Nariño",
    toupper(x) == "CORDOBA" ~ "Córdoba",
    toupper(x) == "TOLIMA" ~ "Tolima",
    toupper(x) == "HUILA" ~ "Huila",
    toupper(x) == "CESAR" ~ "Cesar",
    toupper(x) == "META" ~ "Meta",
    toupper(x) == "MAGDALENA" ~ "Magdalena",
    toupper(x) == "SUCRE" ~ "Sucre",
    toupper(x) == "CAUCA" ~ "Cauca",
    toupper(x) == "RISARALDA" ~ "Risaralda",
    toupper(x) == "CALDAS" ~ "Caldas",
    toupper(x) %in% c("LA GUAJIRA", "GUAJIRA") ~ "La Guajira",
    toupper(x) == "QUINDIO" ~ "Quindío",
    toupper(x) == "CASANARE" ~ "Casanare",
    toupper(x) %in% c("CHOCO", "CHOCÓ") ~ "Chocó",
    toupper(x) == "CAQUETA" ~ "Caquetá",
    toupper(x) == "PUTUMAYO" ~ "Putumayo",
    toupper(x) == "ARAUCA" ~ "Arauca",
    toupper(x) %in% c("SAN ANDRES", "SAN ANDRÉS Y PROVIDENCIA") ~ "San Andrés y Providencia",
    toupper(x) == "GUAVIARE" ~ "Guaviare",
    toupper(x) == "VICHADA" ~ "Vichada",
    toupper(x) == "AMAZONAS" ~ "Amazonas",
    toupper(x) == "GUAINIA" ~ "Guainía",
    toupper(x) == "VAUPES" ~ "Vaupés",
    toupper(x) == "EXTRANJERO" ~ "Extranjero",
    TRUE ~ str_to_title(x)
  )
}

if("S11_estu_depto_reside" %in% names(df)) df$S11_estu_depto_reside <- limpiar_depto_vector(df$S11_estu_depto_reside, FALSE)
if("SP_estu_depto_reside" %in% names(df)) df$SP_estu_depto_reside <- limpiar_depto_vector(df$SP_estu_depto_reside, TRUE)


# ------------------------------------------------------------
# 4.2. Estratos Socioeconómicos (Saber 11)
# ------------------------------------------------------------
if("S11_fami_estratovivienda" %in% names(df)) {
  df$S11_fami_estratovivienda <- limpiar_nulos(df$S11_fami_estratovivienda)
  df$S11_fami_estratovivienda <- case_when(
    grepl("1", df$S11_fami_estratovivienda) ~ "Estrato 1",
    grepl("2", df$S11_fami_estratovivienda) ~ "Estrato 2",
    grepl("3", df$S11_fami_estratovivienda) ~ "Estrato 3",
    grepl("4", df$S11_fami_estratovivienda) ~ "Estrato 4",
    grepl("5", df$S11_fami_estratovivienda) ~ "Estrato 5",
    grepl("6", df$S11_fami_estratovivienda) ~ "Estrato 6",
    toupper(df$S11_fami_estratovivienda) %in% c("SIN ESTRATO", "NINGUNO") ~ "Sin Estrato",
    TRUE ~ df$S11_fami_estratovivienda
  )
}


# ------------------------------------------------------------
# 4.3. Acceso a Internet (Saber 11)
# ------------------------------------------------------------
if("S11_fami_tieneinternet" %in% names(df)) {
  df$S11_fami_tieneinternet <- limpiar_nulos(df$S11_fami_tieneinternet)
  df$S11_fami_tieneinternet <- case_when(
    toupper(df$S11_fami_tieneinternet) %in% c("SI", "SÍ", "S") ~ "Sí",
    toupper(df$S11_fami_tieneinternet) %in% c("NO", "N") ~ "No",
    TRUE ~ df$S11_fami_tieneinternet
  )
}


# ------------------------------------------------------------
# 4.4. Naturaleza y Área de Ubicación del Colegio (Saber 11)
# ------------------------------------------------------------
if("S11_cole_naturaleza" %in% names(df)) {
  df$S11_cole_naturaleza <- limpiar_nulos(df$S11_cole_naturaleza)
  df$S11_cole_naturaleza <- str_to_title(df$S11_cole_naturaleza)
}

if("S11_cole_area_ubicacion" %in% names(df)) {
  df$S11_cole_area_ubicacion <- limpiar_nulos(df$S11_cole_area_ubicacion)
  df$S11_cole_area_ubicacion <- str_to_title(df$S11_cole_area_ubicacion)
}


# ------------------------------------------------------------
# 4.5. Bilingüismo y Calendario Escolar (Saber 11)
# ------------------------------------------------------------
if("S11_cole_bilingue" %in% names(df)) {
  df$S11_cole_bilingue <- limpiar_nulos(df$S11_cole_bilingue)
  df$S11_cole_bilingue <- case_when(
    toupper(df$S11_cole_bilingue) %in% c("SI", "SÍ", "S") ~ "Sí",
    toupper(df$S11_cole_bilingue) %in% c("NO", "N") ~ "No",
    TRUE ~ df$S11_cole_bilingue
  )
}

if("S11_cole_calendario" %in% names(df)) {
  df$S11_cole_calendario <- limpiar_nulos(df$S11_cole_calendario)
  df$S11_cole_calendario <- case_when(
    toupper(df$S11_cole_calendario) == "A" ~ "A",
    toupper(df$S11_cole_calendario) == "B" ~ "B",
    TRUE ~ str_to_title(df$S11_cole_calendario)
  )
}


# ------------------------------------------------------------
# 4.6. Jornada Escolar (Saber 11)
# ------------------------------------------------------------
if("S11_cole_jornada" %in% names(df)) {
  df$S11_cole_jornada <- limpiar_nulos(df$S11_cole_jornada)
  df$S11_cole_jornada <- case_when(
    toupper(df$S11_cole_jornada) == "UNICA" ~ "Única",
    TRUE ~ str_to_title(df$S11_cole_jornada)
  )
}


# ------------------------------------------------------------
# 4.7. Género (Saber Pro)
# ------------------------------------------------------------
if("SP_estu_genero" %in% names(df)) {
  df$SP_estu_genero <- limpiar_nulos(df$SP_estu_genero)
  df$SP_estu_genero <- case_when(
    toupper(df$SP_estu_genero) == "F" ~ "Femenino",
    toupper(df$SP_estu_genero) == "M" ~ "Masculino",
    TRUE ~ str_to_title(df$SP_estu_genero)
  )
}


# ------------------------------------------------------------
# 4.8. Nivel Educativo de los Padres (Saber Pro)
# ------------------------------------------------------------
limpiar_educacion <- function(x) {
  x <- limpiar_nulos(x)
  case_when(
    toupper(x) %in% c("NO SABE", "NO APLICA") ~ NA_character_,
    TRUE ~ x
  )
}

if("SP_fami_educacionmadre" %in% names(df)) df$SP_fami_educacionmadre <- limpiar_educacion(df$SP_fami_educacionmadre)
if("SP_fami_educacionpadre" %in% names(df)) df$SP_fami_educacionpadre <- limpiar_educacion(df$SP_fami_educacionpadre)


# ------------------------------------------------------------
# 4.9. Ingresos Familiares y Situación Laboral (Saber Pro)
# ------------------------------------------------------------
if("SP_fami_ingreso_fmiliar_mensual" %in% names(df)) {
  df$SP_fami_ingreso_fmiliar_mensual <- limpiar_nulos(df$SP_fami_ingreso_fmiliar_mensual)
}

if("SP_estu_trabaja_actualmente" %in% names(df)) {
  df$SP_estu_trabaja_actualmente <- limpiar_nulos(df$SP_estu_trabaja_actualmente)
  df$SP_estu_trabaja_actualmente <- sub("^Si,", "Sí,", df$SP_estu_trabaja_actualmente)
}


# ------------------------------------------------------------
# 4.10. Institución de Educación Superior (Saber Pro)
# ------------------------------------------------------------
if("SP_inst_nombre_institucion" %in% names(df)) {
  df$SP_inst_nombre_institucion <- limpiar_nulos(df$SP_inst_nombre_institucion)
  df$SP_inst_nombre_institucion <- str_to_title(df$SP_inst_nombre_institucion)
  df$SP_inst_nombre_institucion <- gsub("-([A-ZÁÉÍÓÚÑ\\.\\s]+)$", " - \\1", df$SP_inst_nombre_institucion)
}


# ------------------------------------------------------------
# 4.11. Programa Académico (Saber Pro)
# ------------------------------------------------------------
if("SP_estu_prgm_academico" %in% names(df)) {
  df$SP_estu_prgm_academico <- limpiar_nulos(df$SP_estu_prgm_academico)
  df$SP_estu_prgm_academico <- str_to_title(df$SP_estu_prgm_academico)
  df$SP_estu_prgm_academico <- gsub("\\.$", "", df$SP_estu_prgm_academico)
}


# ============================================================
# 5. COMPROBACIÓN MATEMÁTICA Y REPORTE DE FRECUENCIAS
# ============================================================

cat("\n============================================\n")
cat("3. EJECUTANDO VERIFICACIÓN MATEMÁTICA DE FRECUENCIAS...\n")
cat("============================================\n")
cat("  (Comprobando que Registros Válidos + NAs = Total de Filas de la Base)\n\n")

vars_a_verificar <- c(
  "S11_estu_depto_reside", "S11_fami_estratovivienda", "S11_fami_tieneinternet",
  "S11_cole_naturaleza", "S11_cole_area_ubicacion", "S11_cole_bilingue",
  "S11_cole_calendario", "S11_cole_jornada", "SP_estu_genero",
  "SP_estu_depto_reside", "SP_fami_educacionmadre", "SP_fami_educacionpadre",
  "SP_fami_ingreso_fmiliar_mensual", "SP_estu_trabaja_actualmente",
  "SP_inst_nombre_institucion", "SP_estu_prgm_academico"
)

reporte_validacion <- data.frame(
  Variable = character(),
  Validos = numeric(),
  Nulos_NA = numeric(),
  Suma_Total = numeric(),
  Total_Filas_Base = numeric(),
  Estado_Cuadre = character(),
  stringsAsFactors = FALSE
)

for(v in vars_a_verificar) {
  if(v %in% names(df)) {
    n_validos <- sum(!is.na(df[[v]]))
    n_nas <- sum(is.na(df[[v]]))
    sum_conteo <- n_validos + n_nas
    coincide <- (sum_conteo == total_filas_inicial)
    
    reporte_validacion <- rbind(reporte_validacion, data.frame(
      Variable = v,
      Validos = n_validos,
      Nulos_NA = n_nas,
      Suma_Total = sum_conteo,
      Total_Filas_Base = total_filas_inicial,
      Estado_Cuadre = ifelse(coincide, "OK (Cuadrado)", "ERROR")
    ))
  }
}

print(reporte_validacion)


# ============================================================
# 6. EXPORTACIÓN DE LA BASE LIMPIA Y REPORTE DE VALIDACIÓN
# ============================================================

cat("\n============================================\n")
cat("4. EXPORTANDO LA BASE DE DATOS LIMPIA Y VALIDADA...\n")
cat("============================================\n")

directorio_salida <- "Resultados_Directos_S11_SPro"
if (!dir.exists(directorio_salida)) {
  dir.create(directorio_salida, recursive = TRUE)
}

ruta_csv_limpia <- file.path(directorio_salida, "Base_Consolidada_Limpia.csv")
write.csv(df, ruta_csv_limpia, row.names = FALSE)

ruta_excel_val <- file.path(directorio_salida, "Reporte_Validacion_Limpieza.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Validacion_Sumas")
writeData(wb, "Validacion_Sumas", reporte_validacion)
saveWorkbook(wb, ruta_excel_val, overwrite = TRUE)

cat(sprintf("\n¡PROCESO DE LIMPIEZA Y VALIDACIÓN FINALIZADO EXITOSAMENTE!\n"))
cat(sprintf("  -> Base limpia guardada en: %s\n", ruta_csv_limpia))
cat(sprintf("  -> Reporte de validación guardado en: %s\n", ruta_excel_val))