
# Explicacion

# 1) OriginalData
Contiene 2 archivos y una carpeta.
  "PeriodicTable_eng.csv": es una tabla periodica con todo el detalle en ingles. Esta
es la tabla periodica modelo que se toma. Las tablas periodicas en otros idiomas 
son obtenidas modificando esta tabla periodica.
  
  "README.txt": Es un archivo que detalla todo lo relacionado a la carpeta "data-war".
  
  Carpeta "Translate": Dentro de "Translate" se encontrará una subcarpeta por cada idioma que
se quiere utilizar en la aplicacion.
Aqui se encuentran especificaciones de palabras y escritura para los diferentes idiomas.
Por ejemplo, el nombre de cada elemento en español respecto a su contraparte en inglés.
Por cada necesidad de traducción se detalla un archivo.
En cada archivo se enumera las palabras en inglés y su contraparte en el idioma correspondiente.


#######################################################################################################################


2) Carpeta "ExtraData": la forma de escritura en cada idioma de los óxidos no es sencilla de ser colocada en un script en R 
ya que tengo que tener en cuenta toda la gramatica. Entonces, por ejemplo, en vez de deducir el nombre del oaxido a partir del 
nombre del elemento y todas las reglas gramaticales, directamente creamos un archivo que tiene el nombre de cada oxido
en espaniol. Por cada idioma habra un archivo CSV que detalla el nombre de cada oxido. Y los mismo pasara con las otras familias
quimicas. Se tendra entonces un archivo por cada familia quimica, por cada idioma.

###################################################################################################################################

3) Carpeta "Output": esta carpeta contiene en formato CSV todos los objetos que son parte del stock de informacion que la libreria esta usando.
Los objetos son utilizados dentro de la libreria en formato .rds; pero yo los queria tener en formato .csv ya que es un formato más facil de visualizar.
Esta carpeta de "Output" debe quedar vacia cada vez que se ejecuta la Ejecucion de Aurora, para que sean guardados los nuevos archivos.

