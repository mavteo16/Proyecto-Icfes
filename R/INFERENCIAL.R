# ============================================================
# PROYECTO ICFES: MÓDULO DE ESTADÍSTICA INFERENCIAL INTEGRAL
# (MODELOS OLS, VALOR AGREGADO, BRECHAS, HISTOGRAMAS Y REGIONES)
# ============================================================
# Descripción: Script maestro unificado que ajusta modelos OLS, 
# calcula valor agregado institucional y departamental, evalúa brechas
# de género, distribuciones globales y disparidades regionales (S11 & SPro),
# generando visualizaciones de alta calidad y un Excel con todo el sustento numérico.
# ============================================================


# ============================================================
# 1. CONFIGURACIÓN DEL ENTORNO Y LIBRERÍAS
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(openxlsx)
  library(broom)
})

if (!exists("ruta_principal")) {
  ruta_principal <- "Datos"
}

directorio_salida <- "Resultados_Directos_S11_SPro"
directorio_graficos <- file.path(directorio_salida, "Graficos_Inferenciales")

if (!dir.exists(directorio_graficos)) {
  dir.create(directorio_graficos, recursive = TRUE)
}

cat("============================================================\n")
cat("   INICIANDO MÓDULO INFERENCIAL INTEGRAL (S11 & SPRO)        \n")
cat("============================================================\n\n")


# ============================================================
# 2. CARGA Y FILTRADO ESTRICTO DE DATOS
# ============================================================

ruta_base <- file.path(directorio_salida, "Base_Consolidada_Limpia.csv")

if (!file.exists(ruta_base)) {
  stop(sprintf("No se encontró el archivo limpio en: %s. Ejecute los pasos previos.", ruta_base))
}

cat("Leyendo base de datos y filtrando casos completos...\n")
df <- fread(ruta_base, colClasses = "character") %>% as.data.frame()

# Filtrar asegurando que NINGUNA variable clave tenga NA para evitar desajustes
df_model <- df %>%
  mutate(
    s11_global      = suppressWarnings(as.numeric(S11_punt_global)),
    sp_global       = suppressWarnings(as.numeric(SP_punt_global)),
    rezago          = suppressWarnings(as.numeric(rezago_semestres)),
    nse             = suppressWarnings(as.numeric(SP_estu_nse_individual)),
    genero          = SP_estu_genero,
    sp_lectura      = suppressWarnings(as.numeric(SP_mod_lectura_critica_punt)),
    sp_cuantitativo = suppressWarnings(as.numeric(SP_mod_razona_cuantitat_punt)),
    sp_ciudadanas   = suppressWarnings(as.numeric(SP_mod_competen_ciudada_punt)),
    sp_comunicacion = suppressWarnings(as.numeric(SP_mod_comuni_escrita_punt)),
    sp_ingles       = suppressWarnings(as.numeric(SP_mod_ingles_punt))
  ) %>%
  filter(!is.na(s11_global) & !is.na(sp_global) & !is.na(rezago) & !is.na(nse))

cat(sprintf("  -> Registros limpios y sincronizados para modelos: %s\n\n", format(nrow(df_model), big.mark = ",")))


# ============================================================
# 3. ENFOQUE 1: MODELO DE REGRESIÓN LINEAL Y PREDICTIBILIDAD
# ============================================================

cat("3.1. Ajustando modelo de regresión lineal múltiple...\n")

modelo_predictibilidad <- lm(sp_global ~ s11_global + rezago + nse, data = df_model)
resumen_modelo <- summary(modelo_predictibilidad)

print(resumen_modelo)

tabla_coeficientes <- tidy(modelo_predictibilidad)
r_cuadrado <- resumen_modelo$r.squared
r_cuadrado_ajustado <- resumen_modelo$adj.r.squared

cat(sprintf("\n  -> Coeficiente de Determinación (R²): %.4f\n", r_cuadrado))
cat(sprintf("  -> R² Ajustado: %.4f\n\n", r_cuadrado_ajustado))


# ============================================================
# 3.2. VISUALIZACIÓN 1: PREDICTIBILIDAD
# ============================================================

cat("Generando Gráfico 1: Predictibilidad Cole-Universidad...\n")

set.seed(2026)
df_sample <- df_model %>% sample_n(min(5000, nrow(df_model)))

grafico_predictibilidad <- ggplot(df_sample, aes(x = s11_global, y = sp_global)) +
  geom_point(alpha = 0.25, color = "#2b5c8f", size = 1.5) +
  geom_smooth(method = "lm", color = "#d95f02", fill = "#fdb462", linewidth = 1.2, alpha = 0.3) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Predictibilidad del Rendimiento Académico: Cole a Universidad",
    subtitle = sprintf("Modelo OLS: Puntaje Saber Pro vs Saber 11 (R² = %.2f)", r_cuadrado),
    x = "Puntaje Global Saber 11 (Colegio)",
    y = "Puntaje Global Saber Pro (Universidad)",
    caption = "Fuente: Microdatos unificados ICFES (2010-2025). Muestra aleatoria de visualización."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", color = "#333333"),
    panel.grid.major = element_line(color = "#e5e5e5", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g1 <- file.path(directorio_graficos, "1_Predictibilidad_S11_vs_SPro.png")
ggsave(ruta_g1, plot = grafico_predictibilidad, width = 10, height = 6.5, dpi = 300)
cat(sprintf("[OK] Gráfico guardado en: %s\n\n", ruta_g1))


# ============================================================
# 4. ENFOQUE 2: VALOR AGREGADO EDUCATIVO (VALUE-ADDED)
# ============================================================

cat("4.1. Calculando residuales de valor agregado institucional...\n")

df_model$valor_agregado <- residuals(modelo_predictibilidad)

instituciones_va <- df_model %>%
  mutate(institucion = SP_inst_nombre_institucion) %>%
  filter(!is.na(institucion) & institucion != "" & institucion != "NO REPORTADO / NA") %>%
  group_by(institucion) %>%
  summarise(
    Total_Estudiantes = n(),
    Media_Valor_Agregado = mean(valor_agregado, na.rm = TRUE),
    Mediana_Valor_Agregado = median(valor_agregado, na.rm = TRUE),
    Desv_Est = sd(valor_agregado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Total_Estudiantes >= 30) %>%
  arrange(desc(Media_Valor_Agregado))

top_10_instituciones <- head(instituciones_va, 10)


# ============================================================
# 4.2. VISUALIZACIÓN 2: VALOR AGREGADO
# ============================================================

cat("Generando Gráfico 2: Top Instituciones por Valor Agregado...\n")

grafico_valor_agregado <- ggplot(top_10_instituciones, aes(x = reorder(institucion, Media_Valor_Agregado), y = Media_Valor_Agregado)) +
  geom_col(fill = "#2a9d8f", width = 0.7, alpha = 0.9) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top 10 Instituciones de Educación Superior con Mayor Valor Agregado",
    subtitle = "Diferencia promedio entre el puntaje real en Saber Pro y el esperado por el modelo",
    x = NULL,
    y = "Valor Agregado Promedio (Residuales del Modelo)",
    caption = "Nota: Filtro aplicado a instituciones con mínimo 30 estudiantes evaluados en el panel."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "#333333", size = 10),
    panel.grid.major.x = element_line(color = "#e5e5e5", linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g2 <- file.path(directorio_graficos, "2_Valor_Agregado_Institucional.png")
ggsave(ruta_g2, plot = grafico_valor_agregado, width = 11, height = 6, dpi = 300)
cat(sprintf("[OK] Gráfico guardado en: %s\n\n", ruta_g2))


# ============================================================
# 5. ENFOQUE 3: ANÁLISIS DE BRECHAS Y CONTRASTE POR GÉNERO
# ============================================================

cat("5.1. Calculando pruebas de hipótesis y diferencias de medias por género...\n")

df_genero <- df_model %>% filter(genero %in% c("Femenino", "Masculino"))

modulos_a_probar <- c(
  "sp_global"       = "Puntaje Global Saber Pro",
  "sp_lectura"      = "Lectura Crítica",
  "sp_cuantitativo" = "Razonamiento Cuantitativo",
  "sp_ciudadanas"   = "Competencias Ciudadanas",
  "sp_comunicacion" = "Comunicación Escrita",
  "sp_ingles"       = "Inglés"
)

resultados_genero <- data.frame()

for (var_mod in names(modulos_a_probar)) {
  nombre_amable <- modulos_a_probar[var_mod]
  sub_df <- df_genero %>% filter(!is.na(.data[[var_mod]]))
  
  val_fem <- sub_df %>% filter(genero == "Femenino") %>% pull(var_mod)
  val_mas <- sub_df %>% filter(genero == "Masculino") %>% pull(var_mod)
  
  if (length(val_fem) > 30 & length(val_mas) > 30) {
    t_test_res <- t.test(val_fem, val_mas)
    
    media_fem <- mean(val_fem, na.rm = TRUE)
    media_mas <- mean(val_mas, na.rm = TRUE)
    sd_fem <- sd(val_fem, na.rm = TRUE)
    sd_mas <- sd(val_mas, na.rm = TRUE)
    n_fem <- length(val_fem)
    n_mas <- length(val_mas)
    
    s_pooled <- sqrt(((n_fem - 1) * sd_fem^2 + (n_mas - 1) * sd_mas^2) / (n_fem + n_mas - 2))
    d_cohen <- (media_fem - media_mas) / s_pooled
    
    fila_res <- data.frame(
      Modulo                = nombre_amable,
      N_Femenino            = n_fem,
      Media_Femenino        = round(media_fem, 2),
      N_Masculino           = n_mas,
      Media_Masculino       = round(media_mas, 2),
      Diferencia_Medias_F_M = round(media_fem - media_mas, 2),
      Estadistico_t         = round(t_test_res$statistic, 2),
      P_Value               = format.pval(t_test_res$p.value, digits = 4, eps = 0.0001),
      Cohens_d              = round(d_cohen, 3)
    )
    
    resultados_genero <- bind_rows(resultados_genero, fila_res)
  }
}

print(resultados_genero)


# ============================================================
# 5.2. VISUALIZACIÓN 3: BRECHAS POR GÉNERO
# ============================================================

cat("\nGenerando Gráfico 3: Distribución por Módulo y Género...\n")

df_long <- df_genero %>%
  select(genero, all_of(names(modulos_a_probar))) %>%
  pivot_longer(
    cols = all_of(names(modulos_a_probar)),
    names_to = "Modulo_Key",
    values_to = "Puntaje"
  ) %>%
  filter(!is.na(Puntaje)) %>%
  mutate(
    Modulo = recode(Modulo_Key, !!!modulos_a_probar)
  )

colores_genero <- c("Femenino" = "#e76f51", "Masculino" = "#264653")

grafico_brechas <- ggplot(df_long, aes(x = genero, y = Puntaje, fill = genero)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = NA) +
  geom_boxplot(width = 0.15, color = "black", alpha = 0.8, outlier.shape = NA) +
  facet_wrap(~ Modulo, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = colores_genero) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución de Puntajes en Saber Pro según Género",
    subtitle = "Comparativa global y módulos genéricos (Violinplots + Boxplots internos)",
    x = NULL,
    y = "Puntaje Obtenido",
    fill = "Género",
    caption = "Fuente: Microdatos unificados ICFES (2010-2025)."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#555555", margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 10, color = "#333333"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "#e5e5e5", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g3 <- file.path(directorio_graficos, "3_Brechas_Rendimiento_Genero.png")
ggsave(ruta_g3, plot = grafico_brechas, width = 12, height = 7, dpi = 300)
cat(sprintf("[OK] Gráfico guardado en: %s\n\n", ruta_g3))


# ============================================================
# 6. ENFOQUE 4: DISTRIBUCIÓN DE PUNTAJES GLOBALES (S11 VS SPRO)
# ============================================================

cat("7. Generando histogramas y curvas de densidad para puntajes globales...\n")

tabla_sustento_globales <- data.frame(
  Prueba = c("Saber 11 Global", "Saber Pro Global"),
  N_Validos = c(sum(!is.na(df_model$s11_global)), sum(!is.na(df_model$sp_global))),
  Media = c(round(mean(df_model$s11_global, na.rm = TRUE), 2), round(mean(df_model$sp_global, na.rm = TRUE), 2)),
  Mediana = c(round(median(df_model$s11_global, na.rm = TRUE), 2), round(median(df_model$sp_global, na.rm = TRUE), 2)),
  Desv_Estandar = c(round(sd(df_model$s11_global, na.rm = TRUE), 2), round(sd(df_model$sp_global, na.rm = TRUE), 2)),
  Minimo = c(min(df_model$s11_global, na.rm = TRUE), min(df_model$sp_global, na.rm = TRUE)),
  Maximo = c(max(df_model$s11_global, na.rm = TRUE), max(df_model$sp_global, na.rm = TRUE))
)

# A. Histograma y Densidad: Saber 11 Global
grafico_hist_s11 <- ggplot(df_model, aes(x = s11_global)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "#2b5c8f", color = "white", alpha = 0.65) +
  geom_density(color = "#d95f02", linewidth = 1.2) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución del Puntaje Global - Saber 11 (Colegio)",
    subtitle = "Histograma de frecuencias relativas y curva de densidad estimada (Escala 0 - 500)",
    x = "Puntaje Global Saber 11",
    y = "Densidad de Probabilidad",
    caption = "Fuente: Microdatos unificados ICFES (2010-2025)."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#555555", margin = margin(b = 15)),
    panel.grid.major = element_line(color = "#e5e5e5", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g4 <- file.path(directorio_graficos, "4_Histograma_S11_Global.png")
ggsave(ruta_g4, plot = grafico_hist_s11, width = 10, height = 6, dpi = 300)


# B. Histograma y Densidad: Saber Pro Global
grafico_hist_spro <- ggplot(df_model, aes(x = sp_global)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "#2a9d8f", color = "white", alpha = 0.65) +
  geom_density(color = "#e76f51", linewidth = 1.2) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución del Puntaje Global - Saber Pro (Universidad)",
    subtitle = "Histograma de frecuencias relativas y curva de densidad estimada (Escala 0 - 300)",
    x = "Puntaje Global Saber Pro",
    y = "Densidad de Probabilidad",
    caption = "Fuente: Microdatos unificados ICFES (2010-2025)."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#555555", margin = margin(b = 15)),
    panel.grid.major = element_line(color = "#e5e5e5", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g5 <- file.path(directorio_graficos, "5_Histograma_SPro_Global.png")
ggsave(ruta_g5, plot = grafico_hist_spro, width = 10, height = 6, dpi = 300)


# ============================================================
# 7. ENFOQUE 5.1: ANÁLISIS DE DISPARIDADES REGIONALES - SABER PRO
# ============================================================

cat("8.1. Analizando diferencias y desigualdades por departamento en Saber Pro...\n")

df_depto_sp <- df_model %>%
  mutate(depto_sp = SP_estu_depto_reside) %>%
  filter(!is.na(depto_sp) & depto_sp != "" & depto_sp != "NO REPORTADO / NA")

anova_depto_sp <- aov(sp_global ~ as.factor(depto_sp), data = df_depto_sp)
print(summary(anova_depto_sp))

ranking_depto_sp <- df_depto_sp %>%
  group_by(Departamento = depto_sp) %>%
  summarise(
    Total_Estudiantes        = n(),
    Media_Saber_Pro_Global = round(mean(sp_global, na.rm = TRUE), 2),
    Mediana_Saber_Pro      = round(median(sp_global, na.rm = TRUE), 2),
    Media_Saber_11_Global  = round(mean(s11_global, na.rm = TRUE), 2),
    Media_Valor_Agregado   = round(mean(valor_agregado, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  filter(Total_Estudiantes >= 100) %>%
  arrange(desc(Media_Saber_Pro_Global))

cat("Generando Gráfico 6: Ranking Departamental en Saber Pro...\n")

grafico_depto_sp <- ggplot(ranking_depto_sp, aes(x = Media_Saber_Pro_Global, y = reorder(Departamento, Media_Saber_Pro_Global))) +
  geom_segment(aes(x = min(Media_Saber_Pro_Global) - 5, xend = Media_Saber_Pro_Global, 
                   y = Departamento, yend = Departamento), color = "#d3d3d3", linewidth = 0.8) +
  geom_point(color = "#1d3557", size = 3.5, alpha = 0.9) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Ranking Departamental: Puntaje Global Promedio en Saber Pro",
    subtitle = "Comparativa de rendimiento académico universitario por departamento de residencia",
    x = "Puntaje Global Promedio en Saber Pro",
    y = NULL,
    caption = "Fuente: Microdatos unificados ICFES (2010-2025). Filtro: Departamentos con >= 100 estudiantes."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "#333333", size = 9),
    panel.grid.major.x = element_line(color = "#e5e5e5", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g6 <- file.path(directorio_graficos, "6_Ranking_Departamental_SPro.png")
ggsave(ruta_g6, plot = grafico_depto_sp, width = 11, height = 8, dpi = 300)
cat(sprintf("[OK] Gráfico departamental Saber Pro guardado en: %s\n\n", ruta_g6))


# ============================================================
# 7.2. ENFOQUE 5.2: ANÁLISIS DE DISPARIDADES REGIONALES - SABER 11
# ============================================================

cat("8.2. Analizando diferencias y desigualdades por departamento en Saber 11...\n")

df_depto_s11 <- df_model %>%
  mutate(depto_s11 = S11_estu_depto_reside) %>%
  filter(!is.na(depto_s11) & depto_s11 != "" & depto_s11 != "NO REPORTADO / NA")

anova_depto_s11 <- aov(s11_global ~ as.factor(depto_s11), data = df_depto_s11)
print(summary(anova_depto_s11))

ranking_depto_s11 <- df_depto_s11 %>%
  group_by(Departamento = depto_s11) %>%
  summarise(
    Total_Estudiantes       = n(),
    Media_Saber_11_Global = round(mean(s11_global, na.rm = TRUE), 2),
    Mediana_Saber_11      = round(median(s11_global, na.rm = TRUE), 2),
    Desv_Est_S11          = round(sd(s11_global, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  filter(Total_Estudiantes >= 100) %>%
  arrange(desc(Media_Saber_11_Global))

cat("Generando Gráfico 7: Ranking Departamental en Saber 11...\n")

grafico_depto_s11 <- ggplot(ranking_depto_s11, aes(x = Media_Saber_11_Global, y = reorder(Departamento, Media_Saber_11_Global))) +
  geom_segment(aes(x = min(Media_Saber_11_Global) - 5, xend = Media_Saber_11_Global, 
                   y = Departamento, yend = Departamento), color = "#d3d3d3", linewidth = 0.8) +
  geom_point(color = "#e76f51", size = 3.5, alpha = 0.9) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Ranking Departamental: Puntaje Global Promedio en Saber 11",
    subtitle = "Comparativa de rendimiento académico en educación media por departamento de residencia",
    x = "Puntaje Global Promedio en Saber 11",
    y = NULL,
    caption = "Fuente: Microdatos unificados ICFES (2010-2025). Filtro: Departamentos con >= 100 estudiantes."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "#333333", size = 9),
    panel.grid.major.x = element_line(color = "#e5e5e5", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ruta_g7 <- file.path(directorio_graficos, "7_Ranking_Departamental_S11.png")
ggsave(ruta_g7, plot = grafico_depto_s11, width = 11, height = 8, dpi = 300)
cat(sprintf("[OK] Gráfico departamental Saber 11 guardado en: %s\n\n", ruta_g7))


# ============================================================
# 7.3. ENFOQUE 6: EVOLUCIÓN Y VALOR AGREGADO DEPARTAMENTAL (S11 -> SPRO)
# ============================================================

cat("9. Calculando la evolución neta de valor agregado por departamento...\n")

evolucion_departamental <- df_model %>%
  mutate(depto = SP_estu_depto_reside) %>%
  filter(!is.na(depto) & depto != "" & depto != "NO REPORTADO / NA") %>%
  group_by(Departamento = depto) %>%
  summarise(
    Total_Estudiantes         = n(),
    Media_Saber_11            = round(mean(s11_global, na.rm = TRUE), 2),
    Media_Saber_Pro           = round(mean(sp_global, na.rm = TRUE), 2),
    Valor_Agregado_Neto       = round(mean(valor_agregado, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  filter(Total_Estudiantes >= 100) %>%
  mutate(
    Trayectoria = case_when(
      Valor_Agregado_Neto > 0.5  ~ "Supera Expectativa (Mejora Neta)",
      Valor_Agregado_Neto < -0.5 ~ "Bajo Expectativa (Descenso Neta)",
      TRUE                       ~ "Estable / Acorde a lo Esperado"
    )
  ) %>%
  arrange(desc(Valor_Agregado_Neto))

cat("Generando Gráfico 8: Evolución y Valor Agregado Departamental...\n")

grafico_evolucion <- ggplot(evolucion_departamental, aes(x = Valor_Agregado_Neto, y = reorder(Departamento, Valor_Agregado_Neto), color = Valor_Agregado_Neto > 0)) +
  geom_segment(aes(x = 0, xend = Valor_Agregado_Neto, y = Departamento, yend = Departamento), linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("#e76f51", "#2a9d8f"), guide = "none") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Evolución Académica Departamental: De la Educación Media a la Superior",
    subtitle = "Valor agregado neto (Residuales promedio): Departamentos que potencian o rezagan el rendimiento frente a lo esperado",
    x = "Valor Agregado Neto Promedio (Rendimiento Real vs Esperado)",
    y = NULL,
    caption = "Fuente: Microdatos unificados ICFES (2010-2025). Filtro: >= 100 estudiantes."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1a1a1a", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, color = "#555555", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", color = "#333333", size = 9),
    panel.grid.major.x = element_line(color = "#e5e5e5", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 9, color = "#777777", hjust = 1, margin = margin(t = 10))
  )

ggsave(file.path(directorio_graficos, "8_Evolucion_ValorAgregado_Depto.png"), plot = grafico_evolucion, width = 11, height = 8, dpi = 300)
cat("[OK] Gráfico de evolución departamental guardado con éxito.\n\n")


# ============================================================
# 8. EXPORTACIÓN GENERAL A EXCEL (CON TODAS LAS TABLAS DE SUSTENTO)
# ============================================================

cat("10. Exportando todos los resultados y tablas de sustento a Excel (Gráficos 1 al 8)...\n")

ruta_excel_inferencial <- file.path(directorio_salida, "Resultados_Inferenciales_Completos.xlsx")
wb <- createWorkbook()

# Pestaña 1: Coeficientes del Modelo OLS (Soporte Gráfico 1)
addWorksheet(wb, "Modelo_Predictibilidad")
writeData(wb, "Modelo_Predictibilidad", tabla_coeficientes)

# Pestaña 2: Valor Agregado IES (Soporte Gráfico 2)
addWorksheet(wb, "Valor_Agregado_IES")
writeData(wb, "Valor_Agregado_IES", instituciones_va)

# Pestaña 3: Pruebas T y Brechas de Género (Soporte Gráfico 3)
addWorksheet(wb, "Brechas_Genero")
writeData(wb, "Brechas_Genero", resultados_genero)

# Pestaña 4: Estadísticos Globales / Histogramas (Soporte Gráficos 4 y 5)
addWorksheet(wb, "Distribuciones_Globales")
writeData(wb, "Distribuciones_Globales", tabla_sustento_globales)

# Pestaña 5: Ranking Departamental Saber Pro (Soporte Gráfico 6)
addWorksheet(wb, "Depto_Saber_Pro")
writeData(wb, "Depto_Saber_Pro", ranking_depto_sp)

# Pestaña 6: Ranking Departamental Saber 11 (Soporte Gráfico 7)
addWorksheet(wb, "Depto_Saber_11")
writeData(wb, "Depto_Saber_11", ranking_depto_s11)

# Pestaña 7: Evolución y Valor Agregado Departamental (Soporte Gráfico 8)
addWorksheet(wb, "Evolucion_Departamental")
writeData(wb, "Evolucion_Departamental", evolucion_departamental)

saveWorkbook(wb, ruta_excel_inferencial, overwrite = TRUE)

cat(sprintf("[¡ÉXITO!] Módulo inferencial integral finalizado con éxito.\n"))
cat(sprintf("  -> 8 Gráficos de alta calidad generados en: %s\n", directorio_graficos))
cat(sprintf("  -> Archivo Excel con 7 pestañas de sustento numérico guardado en: %s\n", ruta_excel_inferencial))
cat("============================================================\n")