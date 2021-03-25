#' @title stat_ajust
#' @author Maxime Parodi (merci à lui pour son code)
#'
#' @description  Permet d'obtenir les indicateurs d'ajustement (variance, degrés de liberté, p-value, dissimilarité, AIC et BIC) pour les modèles log-linéaires pour lesquels les fonctions classiques ne fonctionnent pas.
#'
#' @param ... le ou les modèles testés
#' @param ref NULL par défaut. Modèle saturé, afin que ses indicateurs AIC et BIC soient nuls.
#'
#' @return un tableau regroupant tous les indicateurs
#' @export
#'
#' @importFrom stats na.omit as.formula lm update.formula
#' @importFrom rlang enquos eval_tidy enquo as_name
#' @importFrom stringr str_sub
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr map2_dfr
#' @importFrom magrittr %>%

stat_ajust <- function(..., ref = NULL) {
  list_glm <- enquos(...)
  noms <- map_chr(list_glm, rlang::as_name)
  list_glm <- map(list_glm, rlang::eval_tidy)

  if (is.null(ref)) {
    aic_ref <- 0
    bic_ref <- 0
  } else {
    mod_ref <- rlang::'!!'(rlang::enquo(ref))
    aic_ref <- mod_ref$deviance - 2 * mod_ref$df.residual
    bic_ref <- mod_ref$deviance - log(sum(mod_ref$y)) * mod_ref$df.residual
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
