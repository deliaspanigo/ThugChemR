
# Great common divisor - Euclides Algorithm
# https://rpubs.com/edisonlmg/MDCyMCM#:~:text=M%C3%A1ximo%20Com%C3%BAn%20Divisor%20(MCD),n%C3%BAmeros%20enteros%20sin%20dejar%20residuo.

#'Great Common Divisor (New Version)
#'
#' This function take numbers and give you the great commond ivisor.
#' s a new version of the original gcd() funtion from 'FRACTION' packages.
#' The original gcd() is only for two numbers, and gdc2() can take any numbers.
#'
#' @param ... Numbers
#' @return Great Commun Divisor
#' @export
#' @examples
#' gcd2(2,4,8)

gcd2 <- function(...){

  numeros = c(...)
  output <- FRACTION::gcd(numeros[1], numeros[2])


  if(length(numeros) >= 3){

    for(n in numeros[3:length(numeros)]) output <- FRACTION::gcd(output, n)
  }

  return(output)
}
