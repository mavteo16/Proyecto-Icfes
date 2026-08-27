# ============================================================
# PROYECTO ICFES: UNIFICACIÓN, CRUCE TOTAL Y ORDENAMIENTO
# VENTANA TEMPORAL: SABER 11 (2010-2025) | SABER PRO (2014-2025)
# ============================================================
# Descripción: Este script integra la base de emparejamientos válidos
# con los microdatos completos de Saber 11 y Saber Pro. Aplica un 
# ordenamiento personalizado, genera auditorías numéricas rigurosas
# y exporta los resultados finales en formato CSV y Excel.
# ============================================================


# ============================================================
# 1. VERIFICACIÓN, INSTALACIÓN Y CARGA AUTOMATIZADA DE LIBRERÍAS
# ============================================================

paquetes_requeridos <- c("data.table", "dplyr", "openxlsx")

for (pkg in paquetes_requeridos) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("La librería '%s' no está instalada. Descargando e instalando automáticamente...", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  library(data.table) # Para lectura ultra rápida de archivos pesados
  library(dplyr)      # Para manipulación y cruce de datos
  library(openxlsx)   # Para exportar reportes estructurados en Excel
})

# Seguridad por si se ejecuta de forma independiente
if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}


# ============================================================
# 2. CONFIGURACIÓN DE RUTAS DE ENTRADA Y SALIDA
# ============================================================

ruta_cruce    <- file.path(ruta_principal, "CRUCE_SABER11_SABERPRO_2010_2025.csv")
ruta_saber11  <- file.path(ruta_principal, "SABER 11 COMPLETO.csv")
ruta_saberpro <- file.path(ruta_principal, "SABER PRO COMPLETO.csv")

directorio_salida <- "Resultados_Directos_S11_SPro"


# ============================================================
# 3. CONFIGURACIÓN DEL ORDEN FINAL DE COLUMNAS (PERSONALIZABLE)
# ============================================================

orden_deseado <- c(
  # BLOQUE 1: IDENTIFICACIÓN SABER 11
  "estu_consecutivo_sb11", 
  "periodo_sb11", 
  
  # BLOQUE 2: SOCIOECONÓMICAS Y CONTEXTO SABER 11
  "S11_estu_estudiante", 
  "S11_estu_depto_reside",
  "S11_fami_estratovivienda", 
  "S11_estu_inse_individual", 
  "S11_fami_tieneinternet",
  "S11_estu_repite",
  "S11_cole_naturaleza",
  "S11_cole_area_ubicacion", 
  "S11_cole_bilingue", 
  "S11_cole_calendario", 
  "S11_cole_jornada", 
  
  # BLOQUE 3: PUNTAJES SABER 11
  "S11_punt_c_naturales", 
  "S11_punt_ingles", 
  "S11_punt_lectura_critica", 
  "S11_punt_matematicas", 
  "S11_punt_sociales_ciudadanas", 
  "S11_punt_global", 
  "S11_percentil_global",
  
  # BLOQUE 4: IDENTIFICACIÓN SABER PRO
  "estu_consecutivo_sbpro", 
  "periodo_sbpro",
  
  # BLOQUE 5: SOCIOECONÓMICAS Y CONTEXTO SABER PRO
  "SP_estu_genero", 
  "SP_estu_depto_reside", 
  "SP_fami_estratovivienda", 
  "SP_estu_nse_individual", 
  "SP_fami_educacionmadre", 
  "SP_fami_educacionpadre", 
  "SP_fami_ingreso_fmiliar_mensual", 
  "SP_fami_tieneinternet", 
  "SP_estu_trabaja_actualmente",
  "SP_inst_nombre_institucion", 
  "SP_estu_prgm_academico", 
  
  # BLOQUE 6: PUNTAJES SABER PRO
  "SP_mod_comuni_escrita_punt", 
  "SP_mod_competen_ciudada_punt", 
  "SP_mod_ingles_punt", 
  "SP_mod_lectura_critica_punt", 
  "SP_mod_razona_cuantitat_punt", 
  "SP_punt_global", 
  "SP_percentil_global"
)


# ============================================================
# 4. EXTRACCIÓN AUTOMATIZADA DE VARIABLES SEGÚN CONFIGURACIÓN
# ============================================================

variables_saber11 <- gsub("^S11_", "", orden_deseado[grepl("^S11_", orden_deseado)])
variables_saber11 <- c("estu_consecutivo", "periodo", variables_saber11)

variables_saberpro <- gsub("^SP_", "", orden_deseado[grepl("^SP_", orden_deseado)])
variables_saberpro <- c("estu_consecutivo", "periodo", variables_saberpro)


# ============================================================
# 5. LECTURA, NORMALIZACIÓN Y PREPARACIÓN DE LAS FUENTES
# ============================================================

cat("\n1. Leyendo bases y normalizando nombres de columnas...\n")

df_cruce <- fread(file = ruta_cruce, colClasses = "character") %>% as.data.frame()
colnames(df_cruce) <- tolower(colnames(df_cruce))

df_s11 <- fread(file = ruta_saber11, colClasses = "character") %>% as.data.frame()
colnames(df_s11) <- tolower(colnames(df_s11))
df_s11 <- df_s11 %>% select(any_of(variables_saber11))

df_sp <- fread(file = ruta_saberpro, colClasses = "character") %>% as.data.frame()
colnames(df_sp) <- tolower(colnames(df_sp))
df_sp <- df_sp %>% select(any_of(variables_saberpro))

cat("2. Limpiando y estandarizando llaves para asegurar un emparejamiento exacto...\n")

df_cruce <- df_cruce %>% mutate(
  estu_consecutivo_sb11  = trimws(toupper(estu_consecutivo_sb11)),
  periodo_sb11           = trimws(toupper(periodo_sb11)),
  estu_consecutivo_sbpro = trimws(toupper(estu_consecutivo_sbpro)),
  periodo_sbpro          = trimws(toupper(periodo_sbpro))
)

df_s11 <- df_s11 %>% mutate(
  estu_consecutivo = trimws(toupper(estu_consecutivo)),
  periodo          = trimws(toupper(periodo))
)

df_sp <- df_sp %>% mutate(
  estu_consecutivo = trimws(toupper(estu_consecutivo)),
  periodo          = trimws(toupper(periodo))
)

colnames(df_s11) <- paste0("S11_", colnames(df_s11))
colnames(df_sp)  <- paste0("SP_", colnames(df_sp))


# ============================================================
# 6. EMPAREJAMIENTO (CRUCE) DE LA INFORMACIÓN
# ============================================================

cat("3. Cruzando la información longitudinalmente...\n")

base_final <- df_cruce %>%
  left_join(df_s11, by = c("estu_consecutivo_sb11" = "S11_estu_consecutivo", 
                           "periodo_sb11"         = "S11_periodo")) %>%
  left_join(df_sp,  by = c("estu_consecutivo_sbpro" = "SP_estu_consecutivo", 
                           "periodo_sbpro"        = "SP_periodo"))


# ============================================================
# 7. APLICACIÓN DEL ORDENAMIENTO ESTRICTO DE COLUMNAS
# ============================================================

cat("4. Ordenando las variables según la configuración establecida...\n")

base_final <- base_final %>%
  select(any_of(orden_deseado), everything())


# ============================================================
# 8. GENERACIÓN DE RESUMEN ESTADÍSTICO Y AUDITORÍA NUMÉRICA
# ============================================================

cat("5. Generando estadísticas descriptivas y auditoría cuantitativa...\n")

vars_num_s11 <- orden_deseado[grepl("S11_punt_|S11_percentil_", orden_deseado)]
vars_num_sp  <- orden_deseado[grepl("SP_mod_.*_punt|SP_punt_|SP_percentil_", orden_deseado)]

generar_stats <- function(df, variables) {
  variables <- variables[variables %in% colnames(df)]
  if(length(variables) == 0) return(data.frame())
  
  resumen <- lapply(variables, function(var) {
    val <- suppressWarnings(as.numeric(df[[var]]))
    val <- val[!is.na(val)]
    if(length(val) == 0) return(NULL)
    data.frame(
      Variable = var,
      N_Validos = length(val),
      N_Perdidos_NA = sum(is.na(suppressWarnings(as.numeric(df[[var]])))),
      Media = round(mean(val), 2),
      Mediana = round(median(val), 2),
      Desv_Estandar = round(sd(val), 2),
      Minimo = min(val),
      Maximo = max(val)
    )
  })
  bind_rows(resumen)
}

stats_saber11  <- generar_stats(base_final, vars_num_s11)
stats_saberpro <- generar_stats(base_final, vars_num_sp)

auditoria <- data.frame(
  Metrica = c(
    "Total Cruces Oficiales", 
    "Cruces con Saber 11 Encontrado", 
    "Cruces con Saber Pro Encontrado",
    "Trayectorias Completas (Ambos Encontrados)"
  ),
  Cantidad = c(
    nrow(base_final), 
    sum(!is.na(base_final$S11_punt_global)), 
    sum(!is.na(base_final$SP_punt_global)),
    sum(!is.na(base_final$S11_punt_global) & !is.na(base_final$SP_punt_global))
  )
)

print(auditoria)


# ============================================================
# 9. EXPORTACIÓN DE RESULTADOS (CSV Y EXCEL)
# ============================================================

cat("6. Exportando archivos consolidados a disco...\n")

if (!dir.exists(directorio_salida)) dir.create(directorio_salida)

# A. Guardar la base sin limpiar en Datos/ (Necesaria para el Paso 6 de anomalías)
ruta_salida_base <- file.path(ruta_principal, "BASE FINAL SIN LIMPIAR.csv")
write.csv(base_final, ruta_salida_base, row.names = FALSE)
cat("[OK] Base final sin limpiar generada en Datos/\n")

# B. Exportar base de datos consolidada limpia en CSV (en la carpeta de resultados)
write.csv(base_final, file.path(directorio_salida, "Base_Consolidada.csv"), row.names = FALSE)

# C. Exportar reporte estadístico y de auditoría en Excel
wb <- createWorkbook()
addWorksheet(wb, "Auditoria_Cruce")
writeData(wb, "Auditoria_Cruce", auditoria)

if(nrow(stats_saber11) > 0) {
  addWorksheet(wb, "Stats_Saber11")
  writeData(wb, "Stats_Saber11", stats_saber11)
}

if(nrow(stats_saberpro) > 0) {
  addWorksheet(wb, "Stats_SaberPro")
  writeData(wb, "Stats_SaberPro", stats_saberpro)
}

saveWorkbook(wb, file.path(directorio_salida, "Resumen_Estadistico.xlsx"), overwrite = TRUE)

cat(sprintf("\n¡PROCESO FINALIZADO EXITOSAMENTE!\nArchivos disponibles en la carpeta: '%s/'\n", directorio_salida))