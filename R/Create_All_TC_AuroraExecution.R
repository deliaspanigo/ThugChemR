#' Create All TC - Ejecucion de Aurora
#'
#' @return The function creates and makes available the databases in .rds
#' format for the ThugChem package.
#' @export
#'
#' @examples
#' if(FALSE) Create_All_TC_EjecucionDeAurora()
Create_All_TC_EjecucionDeAurora <- function(){

  # Push para bases de datos originales
  cat("Push Original Data", "\n")
  Push_DataRaw_to_pkg_TC()
  cat("\n\n")

  #
  # Create and Push for DataTC
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


