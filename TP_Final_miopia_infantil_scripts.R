################################################################################
#                  TRABAJO PRÁCTICO FINAL - Modelado de Miopía                 #
#                  Fundamentos para la Ciencia de Datos - 2025                 #
#                                 Grupo N°: 10                                 #           
################################################################################

rm(list = ls())  # Borra todos los objetos del entorno


# 🟩 1. INSTALACION DE LIBRERIAS
#-------------------------------------------------------------------------------

# Librerías necesarias
libs <- c("readxl", "tidyverse", "ResourceSelection", "caret", "e1071", "car", 
          "kableExtra", "tibble",  "scales", "summarytools","skimr", "MASS", 
          "ggplot2", "tidyr", "tibble", "dplyr", "fmsb", "pROC", "knitr")

# Instalar si faltan
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
}
invisible(lapply(libs, install_if_missing))

# Cargar
invisible(lapply(libs, library, character.only = TRUE))


# 🟩 2. IMPORTACIÓN Y EXPLORACIÓN DE DATOS
#-------------------------------------------------------------------------------

# 🟢 2.1.Descripción de las Variables
#-------------------------------------------------------------------------------

# Carga de datos
datos <- read_excel("chicos25.xlsx", sheet = "datos")  

# Nombres de variables
names(datos) 

# Renombrar columnas para evitar nombres ambiguos antes de la fase de reporte
colnames(datos) <- c(
  "id",
  "miopia",
  "med_spheq",
  "med_al",
  "med_acd",
  "med_vcd",
  "hs_mes_deporte",
  "hs_mes_tv",
  "padre_miopia"
)

# Visión estructural del dataset
str(datos)    

# Se detectaron dos columnas de tipo character, se convierten a tipo factor
datos <- datos |> mutate_if(is.character, as.factor)

# Vista preliminar
kable(head(datos, 6), digits = 2, caption = "Primeras 6 observaciones") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F)

# Descripción general de las variables
summary(dplyr::select(datos, -id))

# Resumen compacto por tipo de variable
skimr::skim(datos)

# Informe HTML detallado con descripción variable por variable
dfSummary(datos |> dplyr::select(-id)) %>%
  print(method = "browser")


# 🟢 2.2. Revisión de Datos Faltantes
#-------------------------------------------------------------------------------

# Contar cuántos valores NA hay por variable
colSums(is.na(datos))

# Verificacion datos duplicados
sum(duplicated(datos))        


# 🟢 2.3. Detección de Valores Atípicos
#-------------------------------------------------------------------------------

# Se seleccionan solo las variables numéricas
datos %>%
  dplyr::select(where(is.numeric), -id) %>%
  pivot_longer(cols = everything(), names_to = "variable", 
               values_to = "valor") %>%
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(fill = "#F8766D", outlier.color = "#619CFF", 
               outlier.shape = 1) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Boxplots de variables numéricas",
       x = "", y = "Valor") +
  theme_minimal()

# Identificar valor exacto de outlier en "hs_mes_deporte"
boxplot.stats(datos$hs_mes_deporte)$out

# Se identifica la observación con la mayor cantidad de "hs_mes_deporte" registrada en el conjunto de datos.
datos %>% 
  filter(hs_mes_deporte == max(hs_mes_deporte, na.rm = TRUE)) %>%
  kable(caption = "Observación con el máximo valor de horas mensuales de actividad física") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE)

# Se detectó un valor atípico de 450 horas mensuales de actividad física, lo cual se considera implausible 
# para un niño. Dado que el resto de los datos de la observación son válidos, se reemplazó por NA 
# únicamente en esa variable.
datos <- datos %>%
  mutate(hs_mes_deporte = ifelse(hs_mes_deporte > 400, NA, hs_mes_deporte))

# Se creó un subconjunto de datos (datos_modelo) eliminando observaciones con valores faltantes, en este 
# caso el que se creo del dato atipico de 450hs de deporte por mes. 

# Solo casos completos
datos_modelo <- datos %>% na.omit()

# Confirmar dimensiones
dim(datos_modelo)

# 🟢 2.4. Recodificación de Variables Categóricas
#-------------------------------------------------------------------------------
# Se unifican las respuestas de las variables miopia y padre_miopia en un formato estándar (respetando 
# tildes y uso de mayúsculas), se eliminan espacios innecesarios y se convierten en factores con niveles 
# definidos ("No", "Sí"). Esto es fundamental para asegurar una correcta interpretación por parte de los 
# modelos estadísticos y evitar errores en los análisis posteriores.

# Pasar a texto plano
datos_modelo$miopia <- as.character(datos_modelo$miopia)
datos_modelo$padre_miopia <- as.character(datos_modelo$padre_miopia)

# Limpiar espacios y unificar tildes
datos_modelo$miopia <- trimws(tolower(datos_modelo$miopia))
datos_modelo$padre_miopia <- trimws(tolower(datos_modelo$padre_miopia))

# Recodificar a formato estándar con tilde
datos_modelo$miopia <- ifelse(datos_modelo$miopia %in% c("si", "sí"), "Sí",
                              ifelse(datos_modelo$miopia == "no", "No", NA))
datos_modelo$padre_miopia <- ifelse(datos_modelo$padre_miopia %in% c("si", "sí"), "Sí",
                                    ifelse(datos_modelo$padre_miopia == "no", "No", NA))

# Convertir a factor con niveles definidos
datos_modelo$miopia <- factor(datos_modelo$miopia, levels = c("No", "Sí"))
datos_modelo$padre_miopia <- factor(datos_modelo$padre_miopia, levels = c("No", "Sí"))

# Verificar que ahora esté bien
table(datos_modelo$miopia, useNA = "ifany")
table(datos_modelo$padre_miopia, useNA = "ifany")


# 🟩 3. DIVISIÓN EN ENTRENAMIENTO Y TEST
#-------------------------------------------------------------------------------

# Se procedió a dividir el dataset limpio (datos_modelo) en subconjuntos de entrenamiento y validación en 
# proporción 70/30. Dado que la variable miopia está desbalanceada (537 “No” y 81 “Sí”), se utilizó un método 
# de partición estratificada mediante createDataPartition() para asegurar que ambas clases estén representadas 
# proporcionalmente en cada conjunto.


# 🟢 3.1. Generación de la Partición Estratificada
#-------------------------------------------------------------------------------

# para que sea reproducible
set.seed(456) 

# Partición estratificada según la variable de respuesta
index <- createDataPartition(datos_modelo$miopia, p = 0.7, list = FALSE)

# Dividir en entrenamiento y validación
train <- datos_modelo[index, ]
test <- datos_modelo[-index, ]


# 🟢 3.2. Verificación de la Distribución de Clases
#-------------------------------------------------------------------------------

# Confirmar la distribución
table(train$miopia)
table(test$miopia)

# Se verifican las cantidades resultantes de cada clase en los conjuntos generados, confirmando que 
# la partición conservó las proporciones originales de la variable objetivo. Observandose que el conjunto 
# de entrenamiento quedó conformado por 433 casos, y el de validación por 184 casos.


# 🟢 3.3. Visualización de la Distribución
#-------------------------------------------------------------------------------

# Crear tabla con distribución real
tabla_particion <- data.frame(
  Conjunto = c("Entrenamiento", "Entrenamiento", "Validación", "Validación"),
  Clase = c("No", "Sí", "No", "Sí"),
  Cantidad = c(376, 57, 160, 24),
  Porcentaje = c(
    round(376 / (376 + 57) * 100, 1),
    round(57 / (376 + 57) * 100, 1),
    round(160 / (160 + 24) * 100, 1),
    round(24 / (160 + 24) * 100, 1)
  )
)

# Convertir porcentaje a texto con símbolo %
tabla_particion$Porcentaje <- paste0(tabla_particion$Porcentaje, "%")

# Mostrar tabla con columnas separadas
kable(
  tabla_particion[, c("Conjunto", "Clase", "Cantidad", "Porcentaje")],
  col.names = c("Conjunto", "Clase", "Cantidad", "Porcentaje"),
  caption = "Distribución de la variable 'miopía' en los conjuntos de entrenamiento y validación"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)

# Gráfico para ver la  proporción en cada conjunto
ggplot(tabla_particion, aes(x = Conjunto, y = Cantidad, fill = Clase)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "#619CFF", "Sí" = "#F8766D")) +
  labs(title = "Proporción de miopía en los conjuntos de entrenamiento y
       validación", y = "Proporción", x = "") +
  theme_minimal()


# 🟩 4. MODELOS DE REGRESIÓN LOGÍSTICA
#-------------------------------------------------------------------------------

# 🟢 4.1. Modelo con todas las variables
#-------------------------------------------------------------------------------
# Ajuste del modelo de regresión logística completo
# Se estima un modelo logístico utilizando como variable dependiente la miopía
# e incluyendo todas las variables explicativas seleccionadas.
modeloCompleto <- glm(miopia ~ med_spheq + med_al + med_acd + med_vcd + 
                        hs_mes_deporte + hs_mes_tv + padre_miopia, 
               data = train, family = binomial)

# Resumen del modelo ajustado
summary(modeloCompleto)


# 🟢 4.2. Modelo con selección automática
#-------------------------------------------------------------------------------
# Selección automática de variables mediante stepwise (AIC)
# Se aplica el procedimiento stepwise en ambas direcciones (hacia adelante y hacia atrás)
# para encontrar un modelo más parsimonioso con menor AIC.
modelo2 <- stepAIC(modeloCompleto, direction = "both", trace = FALSE)

# Resumen del modelo seleccionado
summary(modelo2)


# 🟢 4.3. Modelo alternativo basado en criterios teóricos y simplicidad.
#-------------------------------------------------------------------------------
# Ajuste de un modelo reducido de regresión logística
# Se incluye un subconjunto de variables explicativas seleccionadas manualmente
# considerando su relevancia estadística y/o teórica.
modelo3 <- glm(miopia ~ med_spheq + med_al + med_vcd + padre_miopia, 
               data = train, family = binomial)

# Resumen del modelo reducido
summary(modelo3)


# 🟢 4.4. Comparación de los modelos
#-------------------------------------------------------------------------------
# Comparación de modelos mediante análisis de desviancia
# Se comparan los tres modelos: completo, stepwise (modelo2) y reducido manual (modelo3).
# El test de Chi-cuadrado entre modeloCompleto y modelo2 no es significativo (p = 0.787),
# lo que indica que el modelo2, más simple, no pierde capacidad explicativa relevante.
anova(modeloCompleto,modelo2,modelo3)

# Comparación de modelos según el criterio de AIC
# El modelo2 presenta el AIC más bajo (230.75), lo que sugiere que es el modelo
# con mejor balance entre ajuste y complejidad. 
# Por lo tanto, el modelo stepwise (modelo2) se considera el más adecuado.
AIC(modeloCompleto,modelo2,modelo3)


# 🟩 5. ESTUDIO DEL MODELO ELEGIDO: MODELO 2 MÈTODO STEPWISE
#-------------------------------------------------------------------------------

# 🟢 5.1. Variables significativas
#-------------------------------------------------------------------------------
# Evaluación de la significancia de los predictores
# Se examina el resumen del modelo para identificar las variables que resultaron
# estadísticamente significativas (p < 0.05), lo que indica su aporte al modelo.
summary(modelo2)


# 🟢 5.2. Test de bondad de ajuste
#-------------------------------------------------------------------------------
# Test de bondad de ajuste de Hosmer-Lemeshow
# Evalúa si el modelo ajustado se ajusta adecuadamente a los datos observados.
hoslem.test(ifelse(train$miopia == "Sí", 1, 0), fitted(modelo2))

# Resultado:
# El p-valor alto (> 0.05) indica que no hay evidencia para rechazar la hipótesis de buen ajuste, 
# por lo que el modelo ajusta bien.


# 🟢 5.3. Interpretación de un coeficiente en términos de odds
#-------------------------------------------------------------------------------
# Cálculo del odds ratio para la variable padre_miopia
# Se extrae el coeficiente estimado para la categoría "padre_miopía = Sí" en el modelo
# y se calcula su odds ratio (e^coef), que indica cuánto aumenta (o disminuye) la probabilidad
# de miopía en el niño si el padre también tiene miopía.
names(coef(modelo2))
coef_padre <- coef(summary(modelo2))["padre_miopiaSí", "Estimate"]
odds_ratio <- exp(coef_padre)

# Ver resultados
cat("Coeficiente (Estimate) de padre_miopiaSí:", coef_padre, "\n")
cat("Odds ratio asociado:", odds_ratio, "\n")

# Resultado:
# Un odds ratio de aproximadamente 2.03 indica que tener un padre miope
# duplica la probabilidad de que el niño tenga miopía, comparado con no tenerlo,
# manteniendo todo lo demás constante.


# 🟢 5.4. Multicolinealidad
#-------------------------------------------------------------------------------
# Se calcula el VIF (Factor de Inflación de la Varianza) para cada variable del modelo.
# Valores mayores a 5 (o más estrictamente, a 10) podrían indicar colinealidad preocupante
# entre predictores, lo cual puede afectar la estabilidad del modelo.
vif(modelo2)

# Resultado:
# Todos los valores están muy por debajo del umbral crítico de 5, lo que indica que no existe 
# problema de multicolinealidad significativa entre las variables explicativas del modelo.


# 🟢 5.5. Datos influyentes
#-------------------------------------------------------------------------------

# Calcular Cook's distance
cooksd <- cooks.distance(modelo2)

# Crear un data frame con los valores
cooks_df <- data.frame(
  observacion = 1:length(cooksd),
  cooks_distance = cooksd
)

# Umbral típico para valores influyentes
umbral <- 4 / (nrow(train) - length(modelo2$coefficients))

# Agregar columna para destacar si es influyente
cooks_df$influyente <- cooksd > umbral

# Gráfico con ggplot2
ggplot(cooks_df, aes(x = observacion, y = cooks_distance)) +
  geom_bar(stat = "identity", fill = ifelse(cooks_df$influyente, "#619CFF", "#F8766D")) +
  geom_hline(yintercept = umbral, color = "red", linetype = "dashed") +
  labs(title = "Distancia de Cook por observación",
       x = "Observación",
       y = "Distancia de Cook") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# 🟩 6. MODELO NAIVE BAYES
#-------------------------------------------------------------------------------

# 🟢 6.1. Ajuste del modelo Naive Bayes
#-------------------------------------------------------------------------------
modeloNB <- naiveBayes(miopia ~ ., data = train)

# Ajustar Naive Bayes con las variables del Modelo 2
modelo_nb <- naiveBayes(miopia ~ med_spheq + med_vcd + hs_mes_deporte + padre_miopia, data = train)

# Resumen del modelo
print(modelo_nb)

# 🟡 Tabla resumen variabes numericas

# Extraer parámetros del modelo
params <- modelo_nb$tables

# Variables numéricas (medias y desvíos)
vars_numericas <- c("med_spheq", "med_vcd", "hs_mes_deporte")

tabla_numericas <- lapply(vars_numericas, function(var) {
  df <- as.data.frame(params[[var]])
  df$Clase <- rownames(df)
  df$Variable <- var
  colnames(df)[1:2] <- c("Media", "Desvío")
  df[, c("Variable", "Clase", "Media", "Desvío")]
}) %>%
  bind_rows()

# Mostrar tabla formateada
tabla_numericas %>%
  mutate(across(c(Media, Desvío), ~ round(., 3))) %>%
  kable(
    caption = "Distribución de variables numéricas según la clase (Naive Bayes)",
    col.names = c("Variable", "Clase", "Media", "Desvío Estándar"),
    align = "lccr",
    row.names = FALSE
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# 🟡 Tabla resumen padre_miopia
# Crear tabla padre_miopia
tabla_padre <- as.data.frame(modelo_nb$tables[["padre_miopia"]])

# Pivotar para formato ancho
tabla_padre_pivot <- tabla_padre %>%
  tidyr::pivot_wider(names_from = Y, values_from = Freq)

# Formatear y seleccionar columnas para mostrar
tabla_padre_wide <- tabla_padre_pivot %>%
  mutate(
    `P(miopía = No)` = scales::percent(No, accuracy = 0.1),
    `P(miopía = Sí)` = scales::percent(Sí, accuracy = 0.1)
  ) %>%
  dplyr::select(padre_miopia, `P(miopía = No)`, `P(miopía = Sí)`)

# Mostrar tabla
tabla_padre_wide %>%
  kable(
    caption = "Probabilidades condicionales de miopía según padre miope (Naive Bayes)",
    col.names = c("Padre miope", "P(miopía = No)", "P(miopía = Sí)"),
    align = "lcc"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))


# 🟢 6.2. Validación del modelo en conjunto de test
#-------------------------------------------------------------------------------
# Predecir sobre conjunto de test (para validar el modelo):
pred_nb <- predict(modelo_nb, newdata = test)

# Matriz de confusión y métricas:
# Se indica explícitamente que "Sí" (miopía) es la clase positiva, ya que es el evento de interés.
confusionMatrix(pred_nb, test$miopia, positive = "Sí")

# Obtener matriz de confusión del modelo Naive Bayes
cm_nb <- confusionMatrix(pred_nb, test$miopia, positive = "Sí")

# Armar tabla con métricas
metricas_nb <- tibble(
  Métrica = c(
    "Exactitud (Accuracy)",
    "Sensibilidad (Recall)",
    "Especificidad",
    "Valor Predictivo Positivo",
    "Valor Predictivo Negativo",
    "Índice Kappa",
    "Exactitud Balanceada"
  ),
  Valor = c(
    cm_nb$overall["Accuracy"],
    cm_nb$byClass["Sensitivity"],
    cm_nb$byClass["Specificity"],
    cm_nb$byClass["Pos Pred Value"],
    cm_nb$byClass["Neg Pred Value"],
    cm_nb$overall["Kappa"],
    cm_nb$byClass["Balanced Accuracy"]
  )
)

# Mostrar tabla formateada
metricas_nb %>%
  mutate(Valor = ifelse(Métrica == "Índice Kappa", 
                        sprintf("%.3f", Valor), 
                        scales::percent(Valor, accuracy = 0.1))) %>%
  kable(
    caption = "Métricas de evaluación del modelo Naive Bayes sobre el conjunto de test",
    col.names = c("Métrica", "Valor"),
    align = c("l", "r")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Predicción con probabilidades (para evaluar curva ROC)
pred_prob_nb <- predict(modelo_nb, newdata = test, type = "raw")


# 🟢 6.3. Curva ROC y AUC del modelo Naive Bayes
#-------------------------------------------------------------------------------

# Calcular curva ROC
roc_obj <- pROC::roc(test$miopia, pred_prob_nb[, "Sí"], levels = c("No", "Sí"), direction = "<")

# Ajustar márgenes para que no se corte la leyenda
par(mar = c(5, 5, 4, 2))

# Dibujar la curva ROC sin ejes automáticos
pROC::plot.roc(
  roc_obj,
  col = "#F8766D",
  lwd = 2,
  legacy.axes = TRUE,
  main = "Curva ROC – Naive Bayes",
  xlab = "", ylab = "", axes = FALSE
)

# Ejes personalizados en español
axis(1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
mtext("1 - Especificidad", side = 1, line = 2.5)
mtext("Sensibilidad", side = 2, line = 2.5)

# Leyenda con AUC
legend(
  x = 0.6, y = 0.2,
  legend = sprintf("Naive Bayes (AUC = %.3f)", pROC::auc(roc_obj)),
  col = "#F8766D",
  lwd = 2,
  bty = "n"
)

# Resultado:
# El Modelo Naive Bayes logra una precisión general (accuracy) del 87.5%, pero con una sensibilidad 
# muy baja (12.5%) para detectar miopía, lo que implica que detecta correctamente solo una pequeña 
# proporción de los niños con miopía.
# La especificidad es alta (98.8%), indicando una buena capacidad para identificar correctamente los 
# casos sin miopía.
# El valor predictivo positivo (60%) también es moderado, lo que significa que, cuando el modelo 
# predice miopía, acierta en el 60% de los casos.
# El índice Kappa es 0.169, lo que refleja un acuerdo bajo entre las predicciones y la realidad, 
# más allá del azar.
# El AUC de la curva ROC es aproximadamente 0.85, lo que indica buena capacidad discriminativa 
# global del modelo.


# 🟩 7. Evaluación y Comparación de Modelos en el Conjunto Test
#-------------------------------------------------------------------------------

# 🟢 7.1. Curva ROC y AUC
#-------------------------------------------------------------------------------

# ROC del Modelo 2 (Stepwise)
prob_pred_test <- predict(modelo2, newdata = test, type = "response")
roc_log <- pROC::roc(test$miopia, prob_pred_test, levels = c("No", "Sí"), direction = "<")

# ROC del Modelo Naive Bayes
prob_pred_nb <- predict(modelo_nb, newdata = test, type = "raw")
roc_nb <- pROC::roc(test$miopia, prob_pred_nb[, "Sí"], levels = c("No", "Sí"), direction = "<")

# Ajustar márgenes: c(inferior, izquierda, superior, derecha)
par(mar = c(5, 5, 4, 2))  # Aumenta el espacio inferior si se corta la leyenda

# Dibujar la curva ROC del modelo Stepwise 
pROC::plot.roc(
  roc_log,
  col = "#619CFF",
  lwd = 2,
  legacy.axes = TRUE,
  main = "Curvas ROC – Comparación de Modelos",
  xlab = "", ylab = "", axes = FALSE
)

# Personalizar ejes en español
axis(1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
mtext("1 - Especificidad", side = 1, line = 2.5)
mtext("Sensibilidad", side = 2, line = 2.5)

# Superponer curva del modelo Naive Bayes
pROC::lines.roc(roc_nb, col = "#F8766D", lwd = 2)

# Agregar leyenda con AUCs
legend(
  x = 0.542, y = 0.165,
  legend = c(
    sprintf("Stepwise (AUC = %.3f)", pROC::auc(roc_log)),
    sprintf("Naive Bayes (AUC = %.3f)", pROC::auc(roc_nb))
  ),
  col = c("#619CFF", "#F8766D"),
  lwd = 3,
  bty = "n"
)

# 🟢 7.2. Tabla de Clasificación y Métricas
#-------------------------------------------------------------------------------

# Clasificación binaria con umbral 0.5
clase_pred_test <- ifelse(prob_pred_test >= 0.5, "Sí", "No") |> factor(levels = c("No", "Sí"))

# 🟡 Tabla comparativa

# Confusion matrices
cm_log <- confusionMatrix(clase_pred_test, test$miopia, positive = "Sí")
cm_nb  <- confusionMatrix(pred_nb, test$miopia, positive = "Sí")

# Calcular AUC para ambos modelos
auc_log <- auc(roc_log)
auc_nb  <- auc(roc_nb)

# Crear tabla comparativa
comparacion_modelos <- tibble(
  Modelo = c("Logística Stepwise", "Naive Bayes"),
  Exactitud = c(cm_log$overall["Accuracy"], cm_nb$overall["Accuracy"]),
  Kappa = c(cm_log$overall["Kappa"], cm_nb$overall["Kappa"]),
  Sensibilidad = c(cm_log$byClass["Sensitivity"], cm_nb$byClass["Sensitivity"]),
  Especificidad = c(cm_log$byClass["Specificity"], cm_nb$byClass["Specificity"]),
  `Valor Predictivo Positivo` = c(cm_log$byClass["Pos Pred Value"], cm_nb$byClass["Pos Pred Value"]),
  `Valor Predictivo Negativo` = c(cm_log$byClass["Neg Pred Value"], cm_nb$byClass["Neg Pred Value"]),
  AUC = c(auc_log, auc_nb)
)

# Mostrar la tabla con formato
comparacion_modelos %>%
  mutate(across(c(Exactitud, Sensibilidad, Especificidad,
                  `Valor Predictivo Positivo`, `Valor Predictivo Negativo`, AUC),
                ~ scales::percent(., accuracy = 0.1))) %>%
  mutate(Kappa = round(Kappa, 3)) %>%
  kable(
    caption = "Comparación de métricas entre Modelo Stepwise y Naive Bayes",
    col.names = c("Modelo", "Exactitud", "Kappa", "Sensibilidad", "Especificidad",
                  "VPP", "VPN", "AUC"),
    align = c("l", rep("c", 7))
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


# 🟡 Matriz de confusion
# Matriz de confusión para Modelo Stepwise
cm_stepwise <- confusionMatrix(clase_pred_test, test$miopia, positive = "Sí")

# Matriz de confusión para Modelo Naive Bayes
cm_naive <- confusionMatrix(pred_nb, test$miopia, positive = "Sí")

# Mostrar matriz Stepwise
cm_stepwise$table

# Mostrar matriz Naive Bayes
cm_naive$table

# Resultados:
# El Modelo 2 presenta una precisión global del 89.1%, con alta especificidad (96.2%) para
# identificar correctamente a los niños sin miopía, pero una sensibilidad baja (41.7%) para 
# detectar los casos con miopía.
# El índice Kappa de 0.442 indica un acuerdo moderado entre las predicciones y la realidad, 
# mejor que el azar.
# El AUC de 0.873 refleja una buena capacidad discriminativa general. Sin embargo, la 
# sensibilidad baja sugiere que el modelo puede estar subestimando los casos positivos.


# 🟩 8. Elección del Modelo Según el Objetivo
#-------------------------------------------------------------------------------

# 🟢 8.1. Elección del Punto de Corte Óptimo
#-------------------------------------------------------------------------------

# Probabilidades generadas por el Modelo 2 en el conjunto de test
prob_pred_test <- predict(modelo2, newdata = test, type = "response")

# Definir umbrales a evaluar
pc <- seq(from = 0.1, to = 0.9, by = 0.04)

# Inicializar vectores vacíos para guardar métricas
accu <- sens <- spec <- preci <- F1 <- numeric(length(pc))

# Convertir las etiquetas reales a 0/1 para facilitar el cálculo => (0 = No, 1 = Sí)
real <- ifelse(test$miopia == "Sí", 1, 0)

# Calcular métricas para cada punto de corte
for (i in seq_along(pc)) {
  pred_y <- ifelse(prob_pred_test > pc[i], 1, 0)
  
  # Tabla de confusión manual
  confusion <- table(pred_y, real)
  
  VP <- ifelse("1" %in% rownames(confusion) && "1" %in% colnames(confusion), confusion["1", "1"], 0)
  VN <- ifelse("0" %in% rownames(confusion) && "0" %in% colnames(confusion), confusion["0", "0"], 0)
  FP <- ifelse("1" %in% rownames(confusion) && "0" %in% colnames(confusion), confusion["1", "0"], 0)
  FN <- ifelse("0" %in% rownames(confusion) && "1" %in% colnames(confusion), confusion["0", "1"], 0)
  
  accu[i] <- (VP + VN) / (VP + VN + FP + FN)
  sens[i] <- ifelse((VP + FN) > 0, VP / (VP + FN), NA)
  spec[i] <- ifelse((VN + FP) > 0, VN / (VN + FP), NA)
  preci[i] <- ifelse((VP + FP) > 0, VP / (VP + FP), NA)
  F1[i] <- ifelse(!is.na(preci[i]) && !is.na(sens[i]) && (preci[i] + sens[i]) > 0,
                  2 * preci[i] * sens[i] / (preci[i] + sens[i]), NA)
}

# Crear data frame con las métricas por umbral
df_metr <- data.frame(
  Umbral = pc,
  Accuracy = round(accu, 3),
  Precision = round(preci, 3),
  Sensibilidad = round(sens, 3),
  Especificidad = round(spec, 3),
  F1 = round(F1, 3)
)

# Mostrar tabla
print(df_metr)

# Mostrar la tabla 
df_metr %>%
  kable(digits = 3, caption = "Comparación de métricas para distintos umbrales de clasificación") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center")

# Se observa que a medida que el umbral sube, la precisión y la especificidad aumentan, 
# pero la sensibilidad cae drásticamente.

# 🟡 Visualizar cómo varían las métricas según el umbral
df_metr_esp <- df_metr |>
  rename(
    Sensibilidad = Sensibilidad,
    Especificidad = Especificidad,
    Precisión = Precision,
    `Valor F1` = F1,
    `Exactitud` = Accuracy
  )

df_long <- df_metr_esp |>
  pivot_longer(cols = -Umbral, names_to = "Métrica", values_to = "Valor")

ggplot(df_long, aes(x = Umbral, y = Valor, color = Métrica)) +
  geom_line(linewidth = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Métricas según el Umbral de Corte", y = "Valor", x = "Umbral") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))


# 🟢 8.2. Comparación del Modelo Logístico Stepwise con distintos umbrales y Naive Bayes
#-------------------------------------------------------------------------------
# Se comparan las métricas de Naves Bayes con los tres umbrales representativos del Modelo 2: 
# - el más sensible (0.1), 
# - el tradicional (0.5), 
# - y el que ofrece la mayor exactitud observada (0.62).

# Definir modelos a comparar
modelos <- tibble(
  Modelo = c("Logística Stepwise (0.10)", "Logística Stepwise (0.50)", "Logística Stepwise (0.62)", "Naive Bayes"),
  Tipo   = c("log", "log", "log", "nb"),
  Umbral = c(0.10, 0.50, 0.62, NA)
)

# Calcular métricas para cada modelo
resultados <- modelos |> pmap_dfr(function(Modelo, Tipo, Umbral) {
  
  if (Tipo == "log") {
    clase_pred <- ifelse(prob_pred_test >= Umbral, "Sí", "No") |> factor(levels = c("No", "Sí"))
    cm         <- confusionMatrix(clase_pred, test$miopia, positive = "Sí")
    auc_val    <- as.numeric(auc(roc(test$miopia, prob_pred_test, levels = c("No","Sí"))))
  } else {
    cm      <- confusionMatrix(pred_nb, test$miopia, positive = "Sí")
    auc_val <- as.numeric(auc(roc(test$miopia, pred_prob_nb[,"Sí"], levels = c("No","Sí"))))
  }
  
  sensibilidad  <- cm$byClass["Sensitivity"]
  precision     <- cm$byClass["Pos Pred Value"]
  especificidad <- cm$byClass["Specificity"]
  vpn           <- cm$byClass["Neg Pred Value"]
  exactitud     <- cm$overall["Accuracy"]
  kappa         <- cm$overall["Kappa"]
  f1            <- 2 * (precision * sensibilidad) / (precision + sensibilidad)
  
  tibble(
    Modelo       = Modelo,
    Umbral       = ifelse(is.na(Umbral), "-", as.character(Umbral)),
    Exactitud    = exactitud,
    Kappa        = kappa,
    Sensibilidad = sensibilidad,
    Especificidad = especificidad,
    `VPP`        = precision,
    `VPN`        = vpn,
    `F1`         = f1,
    AUC          = auc_val
  )
})

# 🟡 Comparación en tabla
resultados %>%
  mutate(across(c(Exactitud, Sensibilidad, Especificidad, VPP, VPN, F1, AUC),
                ~ scales::percent(., accuracy = 0.1))) %>%
  mutate(Kappa = round(Kappa, 3)) %>%
  kable(
    caption = "Comparación de métricas: Modelo Logístico (en distintos umbrales) vs Naive Bayes",
    col.names = c("Modelo", "Umbral", "Exactitud", "Kappa", "Sensibilidad", "Especificidad", "VPP", "VPN", "F1", "AUC"),
    align = c("l", rep("c", 9))
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

# 🟡 Comparación gráfica
# 1. Seleccionar y preparar los datos relevantes
radar_data <- resultados %>%
  dplyr::select(Modelo, Exactitud, Sensibilidad, Especificidad, VPP, VPN, F1, AUC) %>%
  column_to_rownames("Modelo")

# 2. Escalar los valores entre 0 y 1 para cada columna (normalización Min-Max)
radar_data_scaled <- as.data.frame(lapply(radar_data, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))

# 3. Añadir filas con valores máximos y mínimos requeridos por libreria fmsb
radar_data_scaled <- rbind(
  Max = rep(1, ncol(radar_data_scaled)),
  Min = rep(0, ncol(radar_data_scaled)),
  radar_data_scaled
)

# 4. Aumentar margen derecho para espacio de leyenda
par(mar = c(5, 4, 4, 8))  # bottom, left, top, right

# 5. Graficar radar chart
radarchart(radar_data_scaled,
           axistype = 1,
           pcol = c("#F8766D", "#00BA38", "#619CFF", "#FF61C3"),
           plty = 1, plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "black",
           caxislabels = seq(0,1,0.2), cglwd = 0.8,
           vlcex = 0.8)
# Título
title("Comparación de Desempeño de Modelos 
      de Clasificación en Métricas Normalizadas")

# 6. Agregar leyenda separada a la derecha
legend(x = 1.3, y = 1.1,
       legend = rownames(radar_data),
       col = c("#F8766D", "#00BA38", "#619CFF", "#FF61C3"),
       lty = 1, lwd = 2, bty = "n", cex = 0.8)

# El Modelo 2 con umbral 0.10 se destaca por su alta sensibilidad (87.5%), lo que lo convierte en 
# una buena opción cuando el objetivo es detectar la mayor cantidad posible de niños con miopía, 
# aun a costa de una menor exactitud (72.3%) y bajo valor predictivo positivo (30.4%).
# En contraste, el mismo Modelo 2 con umbral 0.62 y el Modelo Naive Bayes presentan mayor exactitud 
# (89.7% y 87.5%, respectivamente) y alta especificidad (98.1% y 98.8%), pero una sensibilidad mucho 
# más baja (33.3% y 12.5%), lo que implica que pasan por alto una proporción considerable de casos 
# positivos.
# Por lo tanto, la elección del modelo y umbral depende del objetivo deseado: 
# - buscar maximizar la detección de miopía para asegurar tratamiento oportuno, se recomienda el 
# Modelo 2 con umbral 0.10; 
# - minimizar errores generales de clasificación, el Modelo 2 con umbral 0.62 resulta más equilibrado.