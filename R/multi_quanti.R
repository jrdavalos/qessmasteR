#' @title multi_croise
#' @author  Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau détaillant une variable quantitative en fonction d'une liste de variables catégorielles.
#'
#' @param data base de données
#' @param var_princ variable principale (quantitative, en colonnes)
#' @param ... variables catégorielles à croiser avec la variable principale (en lignes)
#' @param moy TRUE par défaut. Moyenne par modalité.
#' @param sd TRUE par défaut. Écart-type par modalité.
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne par modalité.
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param med TRUE par défaut. Médiane par modalité.
#' @param quart TRUE par défaut. Quartiles Q1 et Q3 par modalité.
#' @param minmax TRUE par défaut. Minimum et maximum par modalité.
#' @param eff TRUE par défaut. Effectifs par modalité.
#' @param eff_na FALSE par défaut. Effectifs par modalité en comptant les non-réponses à la place des effectifs.
#' @param NR TRUE par défaut. Supprime les modalités où il n'y a que des non-réponses.
#'
#' @return Un tabyl data.frame regroupant tous les tableaux croisés avec pourcentages et effectifs. Si les pourcentages sont en ligne et que les totaux sont activés alors la ligne de total est nommée "Ensemble" et la colonne de total est nommée "Total" et inversement pour les pourcentages en colonne.
#' @export
#'
#' @importFrom stats quantile
#' @importFrom rlang enquos
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>% group_by summarise select rename filter
#' @importFrom gmodels ci

multi_quanti <- function(data, var_princ, ..., moy = TRUE, sd = TRUE, ic = TRUE, ic_seuil = 0.05,
                         med = TRUE, quart = TRUE, minmax = TRUE, eff = TRUE, eff_na = FALSE, NR = TRUE) {
  if (!moy) ic <- FALSE
  sommaire <- function(var) {
    suppressWarnings(
      tab <- data %>%
        group_by({{var}}) %>%
        summarise(Minimum = min({{var_princ}}, na.rm = T),
                  Maximum = max({{var_princ}}, na.rm = T),
                  Moyenne = mean({{var_princ}}, na.rm = T),
                  `Ecart-type` = round(sd({{var_princ}}, na.rm = T), 2),
                  `IC-` = round(ci({{var_princ}}, alpha = ic_seuil, na.rm = T)[2], 2),
                  `IC+` = round(ci({{var_princ}}, alpha = ic_seuil, na.rm = T)[3], 2),
                  Q1 = quantile({{var_princ}}, probs = 0.25, na.rm = T),
                  Mediane = quantile({{var_princ}}, probs = 0.5, na.rm = T),
                  Q3 = quantile({{var_princ}}, probs = 0.75, na.rm = T),
                  N = ifelse(eff_na, n(), sum(!is.na({{var_princ}}))),
                  Personne = ifelse(is.na(Moyenne), T, F)) %>%
        rename(Modalites = 1)
    )
    if (!moy) tab <- tab %>% select(-Moyenne)
    if (!sd) tab <- tab %>% select(-`Ecart-type`)
    if (!ic) tab <- tab %>% select(-c(`IC-`, `IC+`))
    if (!med) tab <- tab %>% select(-Mediane)
    if (!quart) tab <- tab %>% select(-c(Q1, Q3))
    if (!minmax) tab <- tab %>% select(-c(Minimum, Maximum))
    if (!eff) tab <- tab %>% select(-N)
    if (NR) tab <- tab %>% filter(!Personne)
    tab %>% select(-Personne)
  }
  list_vars <- rlang::enquos(..., .named = TRUE)
  if (length(list_vars) == 0) {
    stop(paste("Pas de variable a croiser avec", deparse(substitute(var_princ))))
  }
  map_dfr(list_vars, ~sommaire(!!.x), .id = "Variables")
}


