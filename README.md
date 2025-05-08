# Análisis de la Brecha Salarial de Género: Modelado y Descomposición de Factores Explicativos

Este repositorio contiene el código en R y los resultados de un análisis estadístico sobre la brecha salarial de género, utilizando datos de la **Encuesta Permanente de Hogares (EPH)**. El estudio se enfoca en la influencia de factores como la educación, la experiencia laboral y otras variables en el ingreso de los asalariados y la desigualdad salarial de genero.

A lo largo del trabajo se realizó:

+ **Exploración de Datos**: Análisis exploratorio para obtener las variables de control utilizadas en la **Función de Ingresos de Mincer**.

+ **Regresión Cuantílica**: Modelado de la influencia de la educación en diferentes percentiles de ingresos.

+ **Método de Heckman**: Corrección del sesgo de selección presente en el género femenino.

+ **Índices de Gini y Theil**: Cuantificación de la brecha salarial y descomposición de Theil utilizando subgrupos como nivel educativo y género.

+ **Descomposición de Oaxaca-Blinder**: Identificación de la proporción de la brecha salarial que puede explicarse por factores observables y la parte que no.

+ **Visualización**: Gráficos claros para comunicar los resultados y un breve análisis de cada uno de ellos.

En caso de querer visualizar el informe completo descargar `analisisEPH.html`

```
/brecha-salarial-genero
├── /data                    # Datos utilizados
├── /scripts                 # Código en R
├── /graphs                  # gráficos
├── README.md                # Este archivo
├── analisisEPH.html         # Informe completo
└── EPH_registro_1T2024.pdf  # Documentacion oficial del dataset usado
```