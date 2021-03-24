#' stat_ajust
#'
#' Permet d'obtenir les indicateurs d'ajustement (variance, degrés de liberté, p-value, dissimilarité, AIC et BIC). Cela a un intérêt notamment concernant les modèles log-linéaires sur tableau de contingence pour lesquels la fonction glm n'arrive pas à produire d'AIC ou de BIC.
#'
#' @param ... le ou les modèles testés
#' @param ref NULL par défaut
#'
#' @return un tableau regroupant tous les indicateurs
#' @export
#'
#' @importFrom stats na.omit as.formula lm update.formula
#' @importFrom rlang enquos eval_tidy enquo
#' @importFrom stringr str_sub
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr map2_dfr
#' @importFrom magrittr %>%
#'
#' @examples

stat_ajust <- function(..., ref = NULL) {
  list_glm <- enquos(...)
  noms <- as.character(list_glm) %>% map_chr(~str_sub(.x, start = 2))
  list_glm <- map(list_glm, rlang::eval_tidy)

  if (is.null(ref)) {
    aic_ref <- 0
    bic_ref <- 0
  } else {
    Mod_ref <- enquo(ref)
    aic_ref <- rlang::eval_tidy(Mod_ref)$deviance - 2 * rlang::eval_tidy(Mod_ref)$df.residual
    bic_ref <- rlang::eval_tidy(Mod_ref)$deviance - log(sum(rlang::eval_tidy(Mod_ref)$y)) * rlang::eval_tidy(Mod_ref)$df.residual
  }

  return(map2_dfr(list_glm, noms, ~ tibble(
    model = .y,
    G2 = .x$deviance,
    ddl = .x$df.residual,
    p.value.G2 = 1 - pchisq(.x$deviance, .x$df.residual),
    dissimilarity = sum(abs(.x$y - .x$fitted.values)) / sum(.x$y) / 2,
    AIC = .x$deviance - 2 * .x$df.residual - aic_ref,
    BIC = .x$deviance - log(sum(.x$y)) * .x$df.residual - bic_ref
  )
  ))
}
