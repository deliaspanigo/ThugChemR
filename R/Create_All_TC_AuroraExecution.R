#' Create All TC - Ejecucion de Aurora
#'
#' @return The function creates and makes available the databases in .rds
#' format for the ThugChem package.
#' @export
#'
#' @examples
#' if(FALSE) Create_All_TC_EjecucionDeAurora()
Create_All_TC_EjecucionDeAurora <- function(){

  cat("Borrando archivos anteriores...", "\n")
  unlink(list.files(path = "./data-raw/Output/", full.names = TRUE), recursive = TRUE)
  unlink(list.files(path = "./data/", full.names = TRUE), recursive = TRUE)
  cat("Despejado!", "\n")


  # 1) Borrar todos los archivos de ./data-raw/output/(Son archivos .csv)
  # 2) Borrar todos los archivos de ./data/             (Son archivos .rda)

  # Push para bases de datos originales
  # - Sera solo la tabla periodica en ingles como "PeriodicTable_eng"
  # - Aqui debo agregar
  cat("Push Original Data (English - PeriodicTable)", "\n")
  Push_DataRaw_to_pkg_TC()
  cat("\n\n")
  #load_all()
  #document()

  cat("Create and Push for DataTC - PeriodicTable", "\n")
  Create_DataTC_01_PeriodicTable()
  cat("\n\n")
  #
  # Create and Push for DataTC
  # - Son todas las tablas periodicas en todos los idiomas
  #   en un objeto llamado "DataTC_02_Elements".
  cat("Create and Push for DataTC - Elements", "\n")
  Create_DataTC_02_Elements()
  cat("\n\n")

  cat("Create and Push for DataTC - Valences", "\n")
  Create_DataTC_03_Valences()
  cat("\n\n")

  cat("Create and Push for DataTC - Oxyde", "\n")
  Create_DataTC_04_Oxyde()
  cat("\n\n")

  # Create and Push for PackTC
  cat("Create and Push for PackTC - Oxyde", "\n")
  Create_PackTC_04_Oxyde()
  cat("\n\n")



}


