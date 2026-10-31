
# =============================================================================
# 1. CONFIGURACION DEL ENTORNO
# =============================================================================
RUTA_CSV    <- "student_performance.csv"   # <- CAMBIAR si el archivo esta en otra ruta
DIR_SALIDA  <- "resultados_R"              # carpeta de salida (se crea si no existe)
SEMILLA     <- 2026

set.seed(SEMILLA)
DIR_TABLAS  <- file.path(DIR_SALIDA, "tablas")
DIR_FIGURAS <- file.path(DIR_SALIDA, "figuras")
for (d in c(DIR_SALIDA, DIR_TABLAS, DIR_FIGURAS)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Si el archivo no se encuentra y la sesion es interactiva, se pide seleccionarlo.
if (!file.exists(RUTA_CSV)) {
  if (interactive()) {
    message("No se encuentra '", RUTA_CSV, "'. Seleccione el archivo CSV.")
    RUTA_CSV <- file.choose()
  } else {
    stop("No se encuentra el archivo '", RUTA_CSV, "'. Ajuste RUTA_CSV.")
  }
}


# =============================================================================
# 2. LIBRERIAS (se instalan solo si faltan)
#    readr   : importar CSV y exportar tablas
#    dplyr   : manipulacion de datos
#    tidyr   : reorganizar tablas a formato largo (para graficos por facetas)
#    ggplot2 : graficos (incluye el paquete scales, usado para formatos)
# =============================================================================
paquetes <- c("readr", "dplyr", "tidyr", "ggplot2")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) {
  message("Instalando paquetes que faltan: ", paste(faltan, collapse = ", "))
  install.packages(faltan)
}
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
})


# =============================================================================
# 3. IMPORTACION DEL ARCHIVO
#    Se lee todo como texto para detectar problemas antes de convertir tipos.
# =============================================================================
crudo <- read_csv(RUTA_CSV,
                  col_types = cols(.default = col_character()),
                  na = character(),            # no se interpreta nada como NA aun
                  trim_ws = FALSE,
                  show_col_types = FALSE)

# =============================================================================
# 4. INSPECCION INICIAL Y ESTRUCTURA
# =============================================================================
message("Filas: ", nrow(crudo), " | Columnas: ", ncol(crudo))
message("Variables: ", paste(names(crudo), collapse = ", "))

# 4.1 Valores que podrian representar ausencias (vacios, espacios, NA, etc.)
codigos_ausente <- c("", "NA", "N/A", "na", "n/a", "Unknown", "unknown", "NaN",
                     "nan", "null", "NULL", "None", "?")
n_codigo_ausente <- sapply(crudo, function(x) sum(trimws(x) %in% codigos_ausente))
n_con_espacios   <- sapply(crudo, function(x) sum(x != trimws(x)))

# 4.2 Conversion a numerico: se comprueba que no se pierde ningun valor
datos_num <- crudo |> mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))
n_no_numerico <- sapply(seq_along(crudo), function(i) {
  sum(is.na(datos_num[[i]]) & !(trimws(crudo[[i]]) %in% codigos_ausente))
})
names(n_no_numerico) <- names(crudo)
if (any(n_no_numerico > 0)) {
  warning("Hay valores no numericos en: ",
          paste(names(n_no_numerico)[n_no_numerico > 0], collapse = ", "))
}

datos <- datos_num   # todas las columnas del fichero son enteras en la version analizada

# Perfil por variable
perfil <- tibble::tibble(
  variable         = names(datos),
  n_distintos      = sapply(datos, function(x) length(unique(x))),
  n_ausentes       = sapply(datos, function(x) sum(is.na(x))),
  pct_ausentes     = round(100 * n_ausentes / nrow(datos), 2),
  n_codigo_ausente = as.integer(n_codigo_ausente),
  n_con_espacios   = as.integer(n_con_espacios),
  minimo           = sapply(datos, min, na.rm = TRUE),
  maximo           = sapply(datos, max, na.rm = TRUE)
)
print(perfil, n = Inf)
write_csv2(perfil, file.path(DIR_TABLAS, "tabla_perfil_variables.csv"))


# =============================================================================
# 5. AUDITORIA DE CALIDAD DE DATOS
# =============================================================================
N_ORIGINAL <- nrow(datos)

# 5.1 Duplicados: el fichero NO tiene identificador, solo se pueden detectar
#     filas identicas en las 16 columnas. No se eliminan en el analisis principal.
n_copias_duplicadas <- sum(duplicated(datos))              # copias adicionales
n_filas_implicadas  <- sum(duplicated(datos) | duplicated(datos, fromLast = TRUE))
message("Copias exactas: ", n_copias_duplicadas,
        " | filas implicadas en algun duplicado: ", n_filas_implicadas)

# 5.2 Reglas de rango/valores validos.
#     Rangos de Age (18-30), Attendance (0-100) y codigos del resto: ver notas del informe.
#     AssignmentCompletion y ExamScore: se asume 0-100 (NO documentado).
reglas <- tibble::tribble(
  ~variable,              ~regla,                          ~min_valido, ~max_valido, ~documentada,
  "Age",                  "18 a 30 anios",                 18,          30,          "Si (Zenodo)",
  "Attendance",           "porcentaje 0-100",              0,           100,         "Supuesta",
  "StudyHours",           "horas/semana entre 0 y 168",    0,           168,         "Supuesta",
  "OnlineCourses",        "recuento no negativo",          0,           Inf,         "Supuesta",
  "AssignmentCompletion", "porcentaje 0-100",              0,           100,         "Supuesta",
  "ExamScore",            "puntuacion 0-100",              0,           100,         "Supuesta",
  "Extracurricular",      "codigo 0/1",                    0,           1,           "No",
  "Internet",             "codigo 0/1",                    0,           1,           "No",
  "Gender",               "codigo 0/1",                    0,           1,           "No",
  "Discussions",          "codigo 0/1",                    0,           1,           "No",
  "EduTech",              "codigo 0/1",                    0,           1,           "No",
  "Resources",            "codigo 0/1/2",                  0,           2,           "No",
  "Motivation",           "codigo 0/1/2",                  0,           2,           "No",
  "StressLevel",          "codigo 0/1/2",                  0,           2,           "No",
  "LearningStyle",        "codigo 0/1/2/3",                0,           3,           "No",
  "FinalGrade",           "codigo 0/1/2/3",                0,           3,           "No"
)
reglas$n_incumplen <- mapply(function(v, lo, hi) {
  x <- datos[[v]]; sum(x < lo | x > hi, na.rm = TRUE)
}, reglas$variable, reglas$min_valido, reglas$max_valido)
# Valores no enteros en variables de codigo
reglas$n_no_enteros <- sapply(reglas$variable, function(v) sum(datos[[v]] != round(datos[[v]]), na.rm = TRUE))
print(reglas, n = Inf)
write_csv2(reglas, file.path(DIR_TABLAS, "tabla_reglas_rangos.csv"))

# 5.3 Valores atipicos por regla de Tukey (1,5 x IQR). Solo se SENALAN, no se eliminan.
vars_cuant <- c("StudyHours", "Attendance", "Age", "OnlineCourses",
                "AssignmentCompletion", "ExamScore")
atipicos <- bind_rows(lapply(vars_cuant, function(v) {
  x <- datos[[v]]
  q <- quantile(x, c(.25, .75), na.rm = TRUE, type = 7); iqr <- q[2] - q[1]
  lo <- q[1] - 1.5 * iqr; hi <- q[2] + 1.5 * iqr
  tibble::tibble(variable = v, q1 = q[1], q3 = q[2], limite_inf = lo, limite_sup = hi,
                 n_fuera = sum(x < lo | x > hi, na.rm = TRUE),
                 pct_fuera = round(100 * n_fuera / length(x), 2),
                 minimo = min(x), maximo = max(x))
}))
print(atipicos)
write_csv2(atipicos, file.path(DIR_TABLAS, "tabla_atipicos_iqr.csv"))

# 5.4 Patron de filas consecutivas casi identicas: se cuenta cuantas filas coinciden con la
#     anterior en las 10 variables distintas de Resources, Extracurricular, Motivation,
#     Internet, Gender y StressLevel (patron observado en las primeras filas del fichero).
nucleo <- setdiff(names(datos), c("Resources", "Extracurricular", "Motivation",
                                  "Internet", "Gender", "StressLevel"))
m_nuc <- as.matrix(datos[, nucleo])
n_consecutivas <- sum(rowSums(m_nuc[-1, , drop = FALSE] == m_nuc[-nrow(m_nuc), , drop = FALSE]) == ncol(m_nuc))
message("Filas que repiten las 10 variables 'nucleo' de la fila anterior: ", n_consecutivas)

# =============================================================================
# 6. CONJUNTOS DE DATOS DE ANALISIS (el CSV original no se modifica)
#    - datos_principal : las 14.003 filas (no hay razon documentada para eliminar)
#    - datos_sin_dup   : sin copias exactas (analisis de sensibilidad)
#    Las variables de codigo pasan a factor; FinalGrade se trata como ordinal.
# =============================================================================
vars_cat <- c("Resources", "Extracurricular", "Motivation", "Internet", "Gender",
              "LearningStyle", "Discussions", "EduTech", "StressLevel")

preparar <- function(d) {
  d |> mutate(across(all_of(vars_cat), ~ factor(.x)),
              FinalGrade_f = factor(FinalGrade, levels = 0:3, ordered = TRUE))
}
datos_principal <- preparar(datos)
datos_sin_dup   <- preparar(distinct(datos))

comparacion_n <- tibble::tibble(
  conjunto = c("Original (fichero)", "Analisis principal", "Sensibilidad sin duplicados"),
  n_filas  = c(N_ORIGINAL, nrow(datos_principal), nrow(datos_sin_dup)),
  criterio = c("Sin cambios", "Sin eliminar filas",
               "Se elimina la copia repetida de filas identicas en las 16 variables")
)
print(comparacion_n)
write_csv2(comparacion_n, file.path(DIR_TABLAS, "tabla_comparacion_n.csv"))

# Relacion FinalGrade - ExamScore (comprobacion de construccion)
rango_exam <- datos |> group_by(FinalGrade) |>
  summarise(n = n(), exam_min = min(ExamScore), exam_max = max(ExamScore), .groups = "drop")
print(rango_exam)
es_funcion <- all(tapply(datos$FinalGrade, datos$ExamScore, function(x) length(unique(x))) == 1)
message("FinalGrade es funcion exacta de ExamScore en este fichero: ", es_funcion)
write_csv2(rango_exam, file.path(DIR_TABLAS, "tabla_examscore_por_finalgrade.csv"))


# =============================================================================
# 7. ANALISIS UNIVARIANTE
# =============================================================================
asimetria <- function(x) {                       # coeficiente de asimetria ajustado (G1)
  n <- length(x); m <- mean(x); s <- sd(x)
  n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
}
curtosis_exceso <- function(x) {                 # curtosis en exceso (0 = normal; -1,2 = uniforme)
  n <- length(x); m <- mean(x)
  (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2 - 3
}
uni_cuant <- bind_rows(lapply(vars_cuant, function(v) {
  x <- datos_principal[[v]]
  q <- quantile(x, c(.25, .75), na.rm = TRUE, type = 7)
  tibble::tibble(variable = v, n_validos = sum(!is.na(x)), n_ausentes = sum(is.na(x)),
                 media = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
                 mediana = median(x, na.rm = TRUE), q1 = q[1], q3 = q[2], iqr = q[2] - q[1],
                 minimo = min(x, na.rm = TRUE), maximo = max(x, na.rm = TRUE),
                 asimetria = asimetria(x[!is.na(x)]),
                 curtosis_exceso = curtosis_exceso(x[!is.na(x)]))
}))
print(uni_cuant)
write_csv2(uni_cuant, file.path(DIR_TABLAS, "tabla_univariante_cuantitativas.csv"))

# Categoricas: porcentajes sobre el total de filas (coincide con casos validos: 0 ausentes)
uni_cat <- bind_rows(lapply(c(vars_cat, "FinalGrade"), function(v) {
  x <- datos[[v]]
  tab <- table(x, useNA = "no")
  tibble::tibble(variable = v, categoria = names(tab), n = as.integer(tab),
                 pct_sobre_validos = round(100 * as.integer(tab) / sum(tab), 2),
                 n_ausentes = sum(is.na(x)))
}))
print(uni_cat, n = Inf)
write_csv2(uni_cat, file.path(DIR_TABLAS, "tabla_univariante_categoricas.csv"))


# =============================================================================
# 8. ANALISIS BIVARIANTE
#    Respuesta: FinalGrade (ordinal 0-3) -> correlacion de Spearman (rangos) y chi-cuadrado.
#    ExamScore (cuantitativa) -> Mann-Whitney / Kruskal-Wallis (distribuciones discretas
#    y acotadas; se comparan rangos). Ajuste de Holm dentro de cada familia de contrastes.
# =============================================================================
spearman_fila <- function(d, v, resp = "FinalGrade") {
  x <- as.numeric(as.character(d[[v]])); y <- d[[resp]]; n <- length(x)
  rho <- cor(x, y, method = "spearman")
  p   <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE)$p.value)
  se  <- sqrt((1 + rho^2 / 2) / (n - 3))          # error tipico de Bonett y Wright
  tibble::tibble(variable = v, rho = rho,
                 ic_inf = tanh(atanh(rho) - 1.96 * se),
                 ic_sup = tanh(atanh(rho) + 1.96 * se), p = p, n = n)
}
tabla_spearman <- function(d) {
  pred <- c("StudyHours", "Attendance", "AssignmentCompletion", "OnlineCourses", "Age",
            "Motivation", "StressLevel", "Resources", "ExamScore")
  t <- bind_rows(lapply(pred, function(v) spearman_fila(d, v)))
  t$p_holm <- NA_real_
  m <- t$variable != "ExamScore"                  # familia exploratoria sin ExamScore
  t$p_holm[m] <- p.adjust(t$p[m], method = "holm")
  t
}
sp_principal <- tabla_spearman(datos_principal)
sp_sin_dup   <- tabla_spearman(datos_sin_dup)
print(sp_principal)

# Matriz de correlaciones de Spearman entre variables cuantitativas
matriz_spearman <- cor(datos[, vars_cuant], method = "spearman")

# Chi-cuadrado FinalGrade x categorica + V de Cramer
tabla_chi <- function(d) {
  vv <- c("Extracurricular", "Internet", "Gender", "Discussions", "EduTech",
          "LearningStyle", "Motivation", "Resources", "StressLevel")
  t <- bind_rows(lapply(vv, function(v) {
    tab <- table(d[[v]], d$FinalGrade)
    ct  <- suppressWarnings(chisq.test(tab, correct = FALSE))
    tibble::tibble(variable = v, chi2 = unname(ct$statistic), gl = unname(ct$parameter),
                   p = ct$p.value,
                   v_cramer = sqrt(unname(ct$statistic) / (sum(tab) * (min(dim(tab)) - 1))),
                   min_esperada = min(ct$expected))
  }))
  t$p_holm <- p.adjust(t$p, method = "holm"); t
}
chi_principal <- tabla_chi(datos_principal)
chi_sin_dup   <- tabla_chi(datos_sin_dup)
print(chi_principal)

# ExamScore segun categorias: Mann-Whitney (2 grupos) o Kruskal-Wallis (>2 grupos)
tabla_exam <- function(d) {
  vv <- c("Extracurricular", "Internet", "Gender", "Discussions", "EduTech",
          "LearningStyle", "Motivation", "Resources", "StressLevel")
  t <- bind_rows(lapply(vv, function(v) {
    g <- d[[v]]; y <- d$ExamScore
    medianas <- paste(paste0(levels(g), ":", tapply(y, g, median)), collapse = " | ")
    if (nlevels(g) == 2) {
      r1 <- rank(y)[g == levels(g)[2]]; n1 <- length(r1); n0 <- sum(g == levels(g)[1])
      U1 <- sum(r1) - n1 * (n1 + 1) / 2            # U del grupo con codigo mayor
      pv <- suppressWarnings(wilcox.test(y ~ g, exact = FALSE, correct = FALSE)$p.value)
      tibble::tibble(variable = v, prueba = "Mann-Whitney", estadistico = U1, p = pv,
                     tamano_efecto = 2 * U1 / (n1 * n0) - 1, medida = "r biserial por rangos",
                     medianas = medianas)
    } else {
      k <- kruskal.test(y ~ g)
      tibble::tibble(variable = v, prueba = "Kruskal-Wallis",
                     estadistico = unname(k$statistic), p = k$p.value,
                     tamano_efecto = unname(k$statistic) / (length(y) - 1),
                     medida = "epsilon cuadrado", medianas = medianas)
    }
  }))
  t$p_holm <- p.adjust(t$p, method = "holm"); t
}
exam_principal <- tabla_exam(datos_principal)
exam_sin_dup   <- tabla_exam(datos_sin_dup)
print(exam_principal)


# Tabla de contingencia: % POR FILAS (cada fila suma 100 %) de FinalGrade segun categoria
contingencia <- bind_rows(lapply(c("StressLevel", "Discussions"), function(v) {
  tab <- table(datos_principal[[v]], datos_principal$FinalGrade)
  pct <- round(100 * prop.table(tab, 1), 1)
  tibble::tibble(variable = v, categoria = rownames(tab), n = as.integer(rowSums(tab)),
                 FinalGrade_0 = pct[, "0"], FinalGrade_1 = pct[, "1"],
                 FinalGrade_2 = pct[, "2"], FinalGrade_3 = pct[, "3"])
}))
print(contingencia)
write_csv2(contingencia, file.path(DIR_TABLAS, "tabla_contingencia_finalgrade_pct_filas.csv"))

# =============================================================================
# 9. EXPORTACION DE TABLAS
# =============================================================================
write_csv2(sp_principal,   file.path(DIR_TABLAS, "tabla_spearman_finalgrade.csv"))
write_csv2(sp_sin_dup,     file.path(DIR_TABLAS, "tabla_spearman_finalgrade_sin_duplicados.csv"))
write_csv2(as.data.frame(matriz_spearman) |> tibble::rownames_to_column("variable"),
           file.path(DIR_TABLAS, "tabla_matriz_spearman.csv"))
write_csv2(chi_principal,  file.path(DIR_TABLAS, "tabla_chi2_finalgrade.csv"))
write_csv2(chi_sin_dup,    file.path(DIR_TABLAS, "tabla_chi2_finalgrade_sin_duplicados.csv"))
write_csv2(exam_principal, file.path(DIR_TABLAS, "tabla_examscore_por_categorias.csv"))
write_csv2(exam_sin_dup,   file.path(DIR_TABLAS, "tabla_examscore_por_categorias_sin_duplicados.csv"))


# =============================================================================
# 10. FIGURAS (ggplot2; paleta Okabe-Ito, apta para daltonismo)
# =============================================================================
col_base <- "#0072B2"
coma <- scales::label_number(decimal.mark = ",", big.mark = ".")
tema <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
guardar_fig <- function(p, nombre, ancho = 6.5, alto = 4) {
  ggsave(file.path(DIR_FIGURAS, nombre), p, width = ancho, height = alto, dpi = 300, bg = "white")
}
n_txt <- format(nrow(datos), big.mark = ".")

histograma <- function(v, etiqueta) {
  x <- datos[[v]]
  ggplot(datos, aes(x = .data[[v]])) +
    geom_histogram(breaks = seq(min(x) - .5, max(x) + .5, by = 1), fill = col_base, colour = "white") +
    geom_vline(xintercept = median(x), linetype = "dashed") +
    annotate("text", x = median(x), y = Inf, vjust = 1.5, hjust = -0.05, size = 3.3,
             label = paste0("mediana = ", median(x))) +
    scale_y_continuous(labels = coma) +
    labs(title = paste0("Distribucion de ", v, " (n = ", n_txt, ")"),
         x = etiqueta, y = "Frecuencia (n de estudiantes)") + tema
}
guardar_fig(histograma("StudyHours", "Horas de estudio por semana"), "figura_01_distribucion_studyhours.png")
guardar_fig(histograma("Attendance", "Asistencia a clase (%)"),      "figura_02_distribucion_attendance.png")
guardar_fig(histograma("ExamScore",  "Puntuacion del examen (ExamScore)"), "figura_03_distribucion_examscore.png")

# Fig. 4: FinalGrade
fg <- datos |> count(FinalGrade) |> mutate(pct = 100 * n / sum(n))
guardar_fig(
  ggplot(fg, aes(x = factor(FinalGrade), y = pct)) +
    geom_col(fill = col_base) +
    geom_text(aes(label = paste0(formatC(pct, format = "f", digits = 1, decimal.mark = ","), " %")),
              vjust = -0.4, size = 3.3) +
    scale_y_continuous(labels = coma, expand = expansion(mult = c(0, .12))) +
    labs(title = "Distribucion de FinalGrade (variable ordinal de 4 niveles)",
         x = "Codigo de FinalGrade (0-3)", y = "Porcentaje de estudiantes (%)") + tema,
  "figura_04_distribucion_finalgrade.png")

# Fig. 5: variables categoricas
largo_cat <- datos |> select(all_of(vars_cat)) |> mutate(across(everything(), as.character)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "categoria") |>
  count(variable, categoria) |> group_by(variable) |> mutate(pct = 100 * n / sum(n)) |> ungroup() |>
  mutate(variable = factor(variable, levels = vars_cat))
guardar_fig(
  ggplot(largo_cat, aes(x = categoria, y = pct)) +
    geom_col(fill = col_base) +
    geom_text(aes(label = round(pct)), vjust = -0.4, size = 3) +
    facet_wrap(~ variable, scales = "free_x", ncol = 3) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(title = "Distribucion porcentual de las variables categoricas (codigos del fichero)",
         x = "Codigo numerico de cada categoria (sin etiquetas documentadas)", y = "%") + tema,
  "figura_05_variables_categoricas.png", 8.5, 7.5)

# Fig. 6: diagramas de caja de las variables cuantitativas
largo_q <- datos |> select(all_of(vars_cuant)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, levels = vars_cuant))
guardar_fig(
  ggplot(largo_q, aes(x = "", y = valor)) +
    geom_boxplot(fill = "#9ECAE1", outlier.size = 0.8, outlier.alpha = 0.5) +
    facet_wrap(~ variable, scales = "free_y", ncol = 3) +
    labs(title = "Diagramas de caja de las variables cuantitativas (escalas propias)",
         x = NULL, y = "Valor") + tema,
  "figura_06_boxplots_cuantitativas.png", 8.5, 6)

# Fig. 7: ExamScore por FinalGrade
guardar_fig(
  ggplot(datos_principal, aes(x = factor(FinalGrade), y = ExamScore)) +
    geom_boxplot(fill = "#9ECAE1", outlier.size = 0.8) +
    labs(title = "ExamScore segun FinalGrade: tramos que no se solapan",
         x = "Codigo de FinalGrade (0-3)", y = "Puntuacion del examen (ExamScore)") + tema,
  "figura_07_examscore_por_finalgrade.png")

# Fig. 8: StudyHours y Attendance por FinalGrade
largo_8 <- datos |> select(FinalGrade, StudyHours, Attendance) |>
  pivot_longer(-FinalGrade, names_to = "variable", values_to = "valor")
guardar_fig(
  ggplot(largo_8, aes(x = factor(FinalGrade), y = valor)) +
    geom_boxplot(fill = "#9ECAE1", outlier.size = 0.8, outlier.alpha = 0.5) +
    facet_wrap(~ variable, scales = "free_y") +
    labs(title = "Horas de estudio y asistencia segun FinalGrade",
         x = "Codigo de FinalGrade (0-3)", y = "Horas por semana (StudyHours) / porcentaje (Attendance)") + tema,
  "figura_08_studyhours_attendance_por_finalgrade.png", 8.5, 4)

# Fig. 9: matriz de Spearman
mm <- as.data.frame(matriz_spearman) |> tibble::rownames_to_column("fila") |>
  pivot_longer(-fila, names_to = "columna", values_to = "rho") |>
  mutate(fila = factor(fila, levels = rev(vars_cuant)), columna = factor(columna, levels = vars_cuant))
guardar_fig(
  ggplot(mm, aes(x = columna, y = fila, fill = rho)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = formatC(rho, format = "f", digits = 2, decimal.mark = ","),
                  colour = abs(rho) > 0.6), size = 3) +
    scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "white"), guide = "none") +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", limits = c(-1, 1),
                         name = "Spearman") +
    labs(title = "Matriz de correlaciones de Spearman (variables cuantitativas)", x = NULL, y = NULL) +
    tema + theme(axis.text.x = element_text(angle = 40, hjust = 1)),
  "figura_09_matriz_spearman.png", 6.8, 5.6)

# Fig. 10: Spearman con FinalGrade (sin ExamScore)
guardar_fig(
  ggplot(filter(sp_principal, variable != "ExamScore"),
         aes(x = rho, y = reorder(variable, -rho))) +
    geom_vline(xintercept = 0) +
    geom_linerange(aes(xmin = ic_inf, xmax = ic_sup), orientation = "y", colour = col_base) +
    geom_point(colour = col_base, size = 2) +
    coord_cartesian(xlim = c(-0.1, 0.1)) +
    labs(title = "Asociacion con FinalGrade (sin ExamScore)",
         x = "rho de Spearman con FinalGrade (IC al 95 %)", y = NULL) + tema,
  "figura_10_spearman_con_finalgrade.png")

# Fig. 11: ExamScore por categoricas
largo_11 <- datos |> select(ExamScore, all_of(vars_cat)) |> mutate(across(all_of(vars_cat), as.character)) |>
  pivot_longer(-ExamScore, names_to = "variable", values_to = "categoria") |>
  mutate(variable = factor(variable, levels = vars_cat))
guardar_fig(
  ggplot(largo_11, aes(x = categoria, y = ExamScore)) +
    geom_boxplot(fill = "#9ECAE1", outlier.size = 0.8) +
    facet_wrap(~ variable, scales = "free_x", ncol = 3) +
    coord_cartesian(ylim = c(35, 105)) +
    labs(title = "ExamScore segun las variables categoricas (codigo en el eje horizontal)",
         x = NULL, y = "ExamScore") + tema,
  "figura_11_examscore_por_categoricas.png", 8.5, 8)


# =============================================================================
# 11. RESUMEN DE INCIDENCIAS Y RESULTADOS CLAVE
# =============================================================================
incidencias <- tibble::tribble(
  ~id, ~incidencia, ~n_afectado, ~clasificacion, ~tratamiento,
  "I1", "Valores ausentes (incluye codigos tipo NA, vacios, espacios)",
        sum(perfil$n_ausentes) + sum(perfil$n_codigo_ausente), "No detectada", "Ninguno necesario",
  "I2", "Filas duplicadas exactas (copias adicionales)", n_copias_duplicadas,
        "Valor sospechoso (sin identificador no se puede confirmar error)",
        "Se mantienen en el analisis principal; sensibilidad sin duplicados",
  "I3", "Valores fuera de los rangos de la tabla de reglas", sum(reglas$n_incumplen),
        "No detectada", "Ninguno necesario",
  "I4", "StudyHours por encima del limite superior de Tukey (36 h)",
        atipicos$n_fuera[atipicos$variable == "StudyHours"],
        "Valido pero poco habitual", "Se conservan; son plausibles",
  "I5", "FinalGrade es funcion exacta de ExamScore", NA_integer_,
        "Incoherencia de diseno (redundancia estructural)",
        "Se excluye ExamScore de la lectura de predictores; se informa",
  "I6", "Filas consecutivas que repiten 10 variables de la fila anterior (posible generacion por bloques)",
        n_consecutivas, "Valor sospechoso (no confirmable con la documentacion)",
        "Se conservan; se advierte en las limitaciones",
  "I7", "Discrepancias con la descripcion inicial: variables Yes/No codificadas 0/1, OnlineCourses 0-20, Age maximo 29, FinalGrade ordinal",
        NA_integer_, "Discrepancia documental",
        "Se analizan los codigos tal como figuran; no se asignan etiquetas no documentadas"
)
print(incidencias, n = Inf)
write_csv2(incidencias, file.path(DIR_TABLAS, "tabla_incidencias_calidad.csv"))


# =============================================================================
# 12. REPRODUCIBILIDAD
# =============================================================================
capture.output(sessionInfo(), file = file.path(DIR_SALIDA, "session_info.txt"))
message("Fin. Resultados en: ", normalizePath(DIR_SALIDA))
sessionInfo()
