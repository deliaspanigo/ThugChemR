
# Esta funcion lo que hace es pasar todas las bases de datos de la carpeta
# data-raw al formato correcto para ser parte del data set del paquete.
# Finalmente los archivos se guardar en la carpeta 'data' con una extion rara.

#' Push Data Raw to packages ThugChem
#'
#' @param data_raw_folder This is the folder from which the .csv files will be
#' taken to be converted to the .rds format and be part of the ThugChem package.
#'
#' @return No devuelve nada. Solo pasa los archivos .csv a .rds y los coloca en
#' la carpeta correcta.
#' @export
#'
#' @examples
#' if(FALSE) Push_DataRaw_to_pkg_TC()
Push_DataRaw_to_pkg_TC <- function(data_raw_folder = "./data-raw/OriginalData/"){


  # data_raw_folder = "./data-raw/"
  # usethis::use_data_raw(data_row_folder)

  the_files <- list.files(data_raw_folder, full.names = F)
  pos_files_csv <- grep(".csv$", the_files)
  the_files <- the_files[pos_files_csv]

  full_path <- list.files(data_raw_folder, full.names = T)
  pos_path_csv <- grep(".csv$", full_path)
  full_path <- full_path[pos_path_csv]

  files_names <- sapply(the_files, function(x){strsplit(x, "[.]")[[1]][1]})

  # Importamos todas las bases de datos
  my.list <- list()
  for (k in 1:length(full_path)){
    my.list[[k]] <- utils::read.csv(file = full_path[k], sep = ";", header = T, dec =".")
  }
  names(my.list) <- files_names

  generator_sentence01 <- "usethis::use_data(_name_, overwrite = TRUE)"
  generator_sentence02 <- "remove(_name_)"
  for(k in 1:length(my.list)){
    obj <- my.list[[k]]
    name <- names(my.list)[k]

    assign(name, obj)
    la_la <- generator_sentence01
    la_la <- gsub("_name_", name, la_la)

    la02 <- generator_sentence02
    la02 <- gsub("_name_", name, la02)

    eval(parse(text = la_la))
    eval(parse(text = la02))
  }

  # # Esto lo saque de internet, y anda.
  # purrr::walk2(my.list, names(my.list), function(obj, name) {
  #   assign(name, obj)
  #   do.call("use_data", list(as.name(name), overwrite = TRUE))
  # })


  # if(verbose) cat("*** Add_RawData_to_pkg_TC() - End", "\n\n")

}




#Push_DataRaw_to_pkg_TC(data_raw_folder = "./data-raw/")
# Push_DataRaw_to_pkg_TC(data_raw_folder = "./data-raw/OriginalData/")

