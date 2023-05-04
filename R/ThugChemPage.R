#' ThugChem - Page
#'
#' @return The ThugCHem Page!
#' @export
#'
#' @examples
#' if(FALSE) ThugChemPage()
ThugChemPage <- function() {

  ui <- ThugChemR::ui_ThugChemPage()


  server <- ThugChemR::server_ThugChemPage

  # viewer <- dialogViewer("Find and Replace", width = 1000, height = 800)
  # runGadget(ui, server, viewer = viewer)

# # #shiny::runGadget(ui, server, viewer = shiny::browserViewer(), port = 3838)
 shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
#
# performRefactor <- function(contents, from, to, useWordBoundaries = TRUE) {
#
#   reFrom <- if (useWordBoundaries)
#     paste("\\b", from, "\\b", sep = "")
#   else
#     from
#
#   reTo <- to
#   matches <- gregexpr(reFrom, contents, perl = TRUE)
#   changes <- sum(unlist(lapply(matches, function(x) {
#     if (x[[1]] == -1) 0 else length(x)
#   })))
#
#   refactored <- unlist(lapply(contents, function(x) {
#     gsub(reFrom, reTo, x, perl = TRUE)
#   }))
#
#   list(
#     refactored = refactored,
#     changes = changes
#   )
# }
#

# ThugChemPage()
