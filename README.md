# Predicción de Miopía Infantil mediante Ciencia de Datos

Este repositorio contiene el trabajo final para la materia **Fundamentos para la Ciencia de Datos** de la Especialización en Ciencia de Datos (UNLaM), bajo la cátedra de la Prof. Silvia N. Pérez.

## 🧪 Tecnologías utilizadas

<p align="left">
  <img src="https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/RMarkdown-2C3E50?style=for-the-badge&logo=rstudio&logoColor=white" />
  <img src="https://img.shields.io/badge/tidyverse-DB4437?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/ggplot2-009E73?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/caret-984ea3?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/e1071-0072B2?style=for-the-badge&logo=r&logoColor=white" />
</p>

## 🎯 Objetivo
Construir y comparar modelos estadísticos para predecir la presencia de miopía en niños, utilizando variables biométricas y de estilo de vida. El análisis se llevó a cabo sobre un dataset clínico real de 618 niños.

## 🧠 Técnicas aplicadas
- Análisis Exploratorio de Datos (EDA)
- Regresión Logística (con selección automática `stepwise`)
- Clasificación Naive Bayes
- Evaluación de modelos mediante métricas como AUC, sensibilidad, especificidad y exactitud

## 📌 Archivos incluidos
- `TP_miopia_infantil.pdf`: Informe final del trabajo práctico
- `TP_Final_miopia_infantil_scripts.R`: Código completo del análisis en R
- `TP_Final_miopia_infantil_scripts.Rmd`: Documento RMarkdown con visualizaciones interactivas y vistas en HTML

## 📊 Principales hallazgos
- El modelo de regresión logística con selección `stepwise` mostró el mejor rendimiento (AUC = 0.873)
- Las variables más significativas fueron:
  - `med_spheq` (equivalente esférico)
  - `hs_mes_deporte` (horas mensuales de deporte)
  - `padre_miopia` (antecedente familiar)
- La sensibilidad y especificidad varían según el umbral elegido, lo cual es clave según el objetivo clínico.

## 👥 Autores

| Yesica Fica Millán        | Florencia Miranda Charca  | Franco Petraroia           |
|---------------------------|---------------------------|----------------------------|
| [LinkedIn](https://www.linkedin.com/in/yesica-fica-millan/) | [LinkedIn](https://www.linkedin.com/in/florencia-charca/) | [LinkedIn](https://www.linkedin.com/in/franco-petraroia/) |


## 📎 Licencia
Este proyecto se comparte con fines académicos y educativos.
