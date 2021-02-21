# unsupervised_learning

## Imputación de missings.

Al hacer un summary del dataset, se puede notar que la columna **RegDens** contiene 1013 filas con valores Nan. Al revisar el valor del resto de las variable en las filas se puede determinar que en muchos casos tienen valor 0. Por esta razón, se decide eliminar las filas que presentan missing values.

## Transformación de los datos
Las columnas MedHHInc, MeanHHSz y RegPop fueron transformadas a numéricas, eliminando el separador decimal y eliminando el símbolo $.

## Tratamiento de outliers
## Estandarización

Para eliminar los outliers, se estandarizan los datos para que en el cálculo de distancias no tengan más peso aquellas variables con mayor variabilidad/rango. Tomamos logaritmos para eliminar dependencias de outliers.

## Elección del número de clústers

* Dendograma
* Calinski
* Harabasz
* Gráfico Silhouette

