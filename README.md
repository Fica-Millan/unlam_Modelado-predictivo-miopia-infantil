# Predicción de Miopía Infantil mediante Ciencia de Datos

Este repositorio contiene el trabajo final para la materia **Fundamentos para la Ciencia de Datos** de la Especialización en Ciencia de Datos (UNLaM), bajo la cátedra de la Prof. Silvia N. Pérez.

## 🧪 Tecnologías utilizadas

Este proyecto fue desarrollado en **R** para análisis exploratorio, visualización de datos y modelado predictivo. A continuación se listan las principales librerías utilizadas, agrupadas por su funcionalidad:

#### 📊 Visualización de datos

![ggplot2](https://img.shields.io/badge/ggplot2-DC5F00?style=flat&logo=r&logoColor=white)
![scales](https://img.shields.io/badge/scales-DC5F00?style=flat&logo=r&logoColor=white)
![fmsb](https://img.shields.io/badge/fmsb-DC5F00?style=flat&logo=r&logoColor=white)

#### 🧹 Manipulación y preparación de datos

![tidyverse](https://img.shields.io/badge/tidyverse-198754?style=flat&logo=r&logoColor=white)
![dplyr](https://img.shields.io/badge/dplyr-198754?style=flat&logo=r&logoColor=white)
![tidyr](https://img.shields.io/badge/tidyr-198754?style=flat&logo=r&logoColor=white)
![tibble](https://img.shields.io/badge/tibble-198754?style=flat&logo=r&logoColor=white)
![readxl](https://img.shields.io/badge/readxl-198754?style=flat&logo=r&logoColor=white)
![skimr](https://img.shields.io/badge/skimr-198754?style=flat&logo=r&logoColor=white)

#### 📋 Reportes y generación de tablas

![kableExtra](https://img.shields.io/badge/kableExtra-0D6EFD?style=flat&logo=r&logoColor=white)
![summarytools](https://img.shields.io/badge/summarytools-0D6EFD?style=flat&logo=r&logoColor=white)
![knitr](https://img.shields.io/badge/knitr-0D6EFD?style=flat&logo=r&logoColor=white)

#### 🧠 Modelado y evaluación

![caret](https://img.shields.io/badge/caret-6F42C1?style=flat&logo=r&logoColor=white)
![e1071](https://img.shields.io/badge/e1071-6F42C1?style=flat&logo=r&logoColor=white)
![MASS](https://img.shields.io/badge/MASS-6F42C1?style=flat&logo=r&logoColor=white)
![car](https://img.shields.io/badge/car-6F42C1?style=flat&logo=r&logoColor=white)
![ResourceSelection](https://img.shields.io/badge/ResourceSelection-6F42C1?style=flat&logo=r&logoColor=white)
![pROC](https://img.shields.io/badge/pROC-6F42C1?style=flat&logo=r&logoColor=white)


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
