#' @title multi_croise
#' @author  Julio Ricardo Davalos
#'
#'
#' @description Permet d'obtenir un tableau croisant une variable d'intérêt avec une liste d'autres variables.
#'
#' @param data base de données
#' @param var_princ variable principale (en colonnes)
#' @param ... variables à croiser avec la variable principale (en lignes)
#' @param NR FALSE par défaut. Compte les non-réponses.
#' @param pct_ligne TRUE par défaut. Pourcentages en ligne, sinon en colonne.
#'
#' @return un tibble regroupant tous les tableaux croisés avec pourcentages et effectifs.
#' @export
#'
#' @importFrom stats na.omit as.formula lm update.formula
#' @importFrom rlang enquos
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>% mutate rename if_else
#' @importFrom janitor %>% tabyl adorn_totals adorn_percentages adorn_pct_formatting adorn_ns

multi_croise <- function(data, var_princ, ..., NR = FALSE, pct_ligne = TRUE) {
  tableau <- function(var){
    data %>%
      tabyl({{var}}, {{var_princ}}, show_na = NR) %>%
      adorn_totals(c("row", "col")) %>%
      adorn_percentages(ifelse(pct_ligne, "row", "col")) %>%
      adorn_pct_formatting() %>%
      adorn_ns() %>%
      tibble %>%
      rename(Modalites = {{var}}) %>%
      {if (pct_ligne) mutate(., Modalites = if_else(Modalites == "Total",
                                                      "Ensemble",
                                                      Modalites))
        else rename(., Ensemble = Total)}
  }

  list_vars <- rlang::enquos(..., .named = TRUE)
  map_dfr(list_vars, ~ tableau(!!.x), .id = "Variables")
}
