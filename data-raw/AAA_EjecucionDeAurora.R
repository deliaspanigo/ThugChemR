
# Este archvio .R contiene la ejecucion de todas
# las funciones que deben correr para generar de cero
# todas las bases de datos de ThugChem e inmediatamente
# pasarlas como bases de datos del package.

# Discriminaremos la creación de dos objetos:
# 1) DataTC: es un data.frame
# 2) PackTC: es una lista.

# Ejemplo de DataTC es DataTC_02_Elements que es un objeto data.frame
# en donde cada fila es un elemento de la tabla periodica.
# El objeto DataTC_03_Valeces es un data frame que contiene en cada fila
# una valencia particular de cada elemento, entonces cada elemento contiene
# tantas filas como valencias posee.

# Un ejemplo de PackTC es PackTC_04_Oxyde que es una lista. Cada elemento de esta
# lista es otra lista con todas las resoluciones para un elemento con una valencia
# en particular.

the_dir <- "./data-raw/"
all_files <- list.files(the_dir)
pos_r_files <- grepl(".R$", all_files)
the_r_files <- all_files[pos_r_files]
dt_aurora <- grepl("^AAA_", the_r_files)
the_r_files <- the_r_files[!dt_aurora]
relative_path_r_files <- paste0(the_dir, the_r_files)


sapply(1:length(relative_path_r_files),  function(x){
  source(relative_path_r_files[x])})

Create_DataTC_02_Elements()
Create_DataTC_03_Valences()
Create_DataTC_04_Oxyde()

Create_PackTC_04_Oxyde()

Push_DataRaw_to_pkg_TC()


