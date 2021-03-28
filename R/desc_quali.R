#' @title  desc_quali (beta)
#' @author Kanto Fiaferana
#'
#' @description Permet d'obtenir une description de plusieurs variables en même temps avec la fonction freq()
#'
#' @param ... variables souhaitées
#'
#' @return un tableau avec les effectifs et fréquences de chaque modalité de chaque variable
#'
#' @importFrom questionr freq
#' @importFrom dplyr bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' data <- data.frame(replicate(3,sample(0:3,1000,rep=TRUE)))
#' desc_quali(data$X1,data$X2,data$X3)

desc_quali <- function(...) {
  args <- list(...)
  z <- lapply(args, function(x) {
    freq(x, total = TRUE, valid = FALSE, exclude = NA)
  })

  z %>% bind_rows %>% rownames_to_column %>% tibble
}
