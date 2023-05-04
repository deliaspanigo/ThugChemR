#' Solver for Oxyde
#'
#' Make the resolution for steciometry oxyde equation, step by step.
#' Each row is a step to obtein the final equation.
#' Return a data.frame, only with coefficients and subindex.
#'
#' @param element_valence The valence of the element
#' @param gas_status_element Logic value. If the elemnt is gas TRUE else FALSE.
#' @return A matrix of the infile
#' @export
#' @examples
#' Solver_Oxyde(element_valence = 1, gas_status_element = TRUE)


Solver_Oxyde <- function(element_valence = 1, gas_status_element){

  # Default values
  # # # element_valence = 1
  # # # gas_status_element = TRUE

  # Special Information
  oxygen_valence <- 2

  # Initialization for objects
  the_step <- 0
  before_step <- c()

  the_cols <- c("coef1", "subER", "coef2", "subOR", "coef3", "subEP", "subOP")
  solver_data <- as.data.frame(matrix(NA, 20, length(the_cols)))
  colnames(solver_data) <- the_cols


  # # # # Steps educativos...
  # # # 1) Presentacion del elemento y del oxigeno, todo con subindices 1.
  # # # 2) Subindice 2 para el oxigeno por ser gas y si corresponde tambien
  # # #    para el elemento en reactivos tambien.
  # # # 3) Presentacion del oxido todo con sunindices 1.
  # # # 4) Subindices en el oxido por las valencias cruzadas de los elementos.

  # # # # # # Los siguientes pasos son efectivamente "SOLVER"...
  # # # 5) Simplificacion de los subindices en el oxido
  # # # 6) Balanceo del oxido
  # # # 7) Balanceo del elemento
  # # # 8) Simplificacion de los coeficientes

  #  #  #  Step 1 de 8) Initial Information
  #  #  #  Presentacion del elemento y del oxigeno, todo con subindices 1.
  {
    # The step
    the_step <- the_step + 1
    before_step <- the_step - 1

    solver_data$coef1[the_step] <- 1
    solver_data$coef2[the_step] <- 1
    solver_data$coef3[the_step] <- 0

    solver_data$subER[the_step] <- 1
    solver_data$subOR[the_step] <- 1
    solver_data$subEP[the_step] <- 0 #1  # Element in Products
    solver_data$subOP[the_step] <- 0   # Oxygen in products

  }


  #  #  #  Step 2 de 8) Initial Information
  #  #  #  Subindice 2 para el oxigeno y si corresponde tambien para el elemento
  #  #  #  en reactivos
  {

    # The step
    the_step <- the_step + 1
    before_step <- the_step - 1

    # Default values - Before Step
    solver_data[the_step, ] <- solver_data[before_step, ]

    # Nuevos valores
    if(gas_status_element)  solver_data$subER[the_step] <- 2
    solver_data$subOR[the_step] <- 2



  }


  #  #  #  Step 3 de 8)
  #  #  # Presentacion del oxido todo con subnindices 1.
  {

    # The step
    the_step <- the_step + 1
    before_step <- the_step - 1

    # Default values - Before Step
    solver_data[the_step, ] <- solver_data[before_step, ]

    # Coeficiente y subindice del oxido. Arranca todo con 1.
    solver_data$coef3[the_step] <- 1
    solver_data$subEP[the_step] <- 1
    solver_data$subOP[the_step] <- 1





  }

  #  #  #  Step 4 de 8)
  #  #  # Subindices en el oxido por las valencias cruzadas de los elementos.
  {

    # The step
    the_step <- the_step + 1
    before_step <- the_step - 1

    # Default values - Before Step
    solver_data[the_step, ] <- solver_data[before_step, ]

    solver_data$subEP[the_step] <- oxygen_valence #1  # Element in Products
    solver_data$subOP[the_step] <- element_valence   # Oxygen in products



  }


  ### ### ###


  #  #  #  Step 5 de 8) Si los subindices del productos se pueden simplificar,
  #  #  #     lo que hacemos es simplificarlos y agregar el MaximoComunDivisor (MCD)
  #  #  #     como coeficiente en produtos.
  #  #  #     Este paso ya conforma la formula quimica

  # The step
  the_step <- the_step + 1
  before_step <- the_step - 1

  # Default values - Before Step
  solver_data[the_step, ] <- solver_data[before_step, ]

  # GCD
  # Look at subindex in product
  the_gcd_01 <- gcd2(solver_data$subEP[before_step], solver_data$subOP[before_step])
  solver_data$subEP[the_step] <- solver_data$subEP[before_step]/the_gcd_01
  solver_data$subOP[the_step] <- solver_data$subOP[before_step]/the_gcd_01
  solver_data$coef3[the_step] <- the_gcd_01

  remove(the_gcd_01)




  #  #  #  Step 6 de 8) Se balancea el oxigeno.
  #  #  #    Si no esta balanceado, se coloca el subindice de oxigeno
  #  #  #    en reactivos como coeficiente en productos y el subindice
  #  #  #    del oxigeno en productos como coeficiente del oxigeno en reactivos

  # The step
  the_step <- the_step + 1
  before_step <- the_step - 1

  # Default values - Before Step
  solver_data[the_step, ] <- solver_data[before_step, ]

  mount_oxygen_r <- solver_data$coef2[before_step] * solver_data$subOR[before_step]
  mount_oxygen_p <- solver_data$coef3[before_step] * solver_data$subOP[before_step]

  if(mount_oxygen_r != mount_oxygen_p){

    solver_data$coef2[the_step] <- solver_data$subOP[before_step]
    solver_data$coef3[the_step] <- solver_data$subOR[before_step]

  }


  remove(mount_oxygen_r, mount_oxygen_p)




  #  #  #  Step 7 de 8) Se balancea el elemento.
  #  #  #    Si no esta balanceado, no se toca los procutos. Solo se cambia
  #  #  #    el coeficiente del elemento en reactivos.

  # The step
  the_step <- the_step + 1
  before_step <- the_step - 1

  # Default values - Before Step
  solver_data[the_step, ] <- solver_data[before_step, ]

  mount_element_r <- solver_data$coef1[before_step] * solver_data$subER[before_step]
  mount_element_p <- solver_data$coef3[before_step] * solver_data$subEP[before_step]
  mod_coef1 <- mount_element_p/mount_element_r

  if(mod_coef1!= 1){

    solver_data$coef1[the_step] <- mod_coef1


  }


  remove(mount_element_r, mount_element_p, mod_coef1)



  #  #  #  Step 8 de 8) De ser posible simplificamos todos los coeficientes
  #  #  #   La evuacion debe llegar a su minima expresion.
  #  #  #

  # The step
  the_step <- the_step + 1
  before_step <- the_step - 1

  # Default values - Before Step
  solver_data[the_step, ] <- solver_data[before_step, ]

  gcd_02 <- gcd2(solver_data$coef1[before_step], solver_data$coef2[before_step],
                 solver_data$coef3[before_step])


  if(gcd_02 != 1){

    solver_data$coef1[the_step] <- solver_data$coef1[before_step]/gcd_02
    solver_data$coef2[the_step] <- solver_data$coef2[before_step]/gcd_02
    solver_data$coef3[the_step] <- solver_data$coef3[before_step]/gcd_02

  }


  remove(gcd_02)



  # Output
  solver_data <- stats::na.omit(solver_data)
  return(solver_data)


}





# Solver_Oxyde(element_valence = 1, gas_status_element = FALSE)

# Resolution_Oxyde(chem_symbol = "Fe", element_valence = 3, gas_status_element = FALSE)


# Arrow
# arrows(gps_x[6], gps_y[6], gps_x[7], gps_y[7], lwd = 9, length = 0.70)
