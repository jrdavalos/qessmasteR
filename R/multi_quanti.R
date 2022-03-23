#' @title multi_quanti
#' @author  Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau détaillant une variable quantitative en fonction d'une liste de variables catégorielles.
#'
#' @param data base de données
#' @param var_princ variable principale (quantitative, en colonnes)
#' @param ... variables catégorielles à croiser avec la variable principale (en lignes)
#' @param moy TRUE par défaut. Moyenne par modalité.
#' @param sd TRUE par défaut. Écart-type par modalité.
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne par modalité. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param nb nombre de décimales pour la moyenne, l'écart-type et l'intervalle de confiance.
#' @param med TRUE par défaut. Médiane par modalité.
#' @param quart TRUE par défaut. Quartiles Q1 et Q3 par modalité.
#' @param minmax TRUE par défaut. Minimum et maximum par modalité.
#' @param eff TRUE par défaut. Effectifs par modalité.
#' @param eff_na FALSE par défaut. Effectifs par modalité en comptant les non-réponses à la place des effectifs. N'apparait pas si eff = FALSE
#' @param NR TRUE par défaut. Supprime les non-réponses
#'
#' @return Un tibble avec en colonne les indicateurs synthétiques de la variable d'intérêt selon les modalités des variables catégorielles choisies.
#' @export
#'
#' @importFrom stats quantile
#' @importFrom rlang enquos
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>% group_by summarise select rename filter n
#' @importFrom gmodels ci

multi_quanti <- function(data, var_princ, ..., moy = TRUE, sd = TRUE, ic = TRUE, ic_seuil = 0.05,
                         nb = 2, med = TRUE, quart = TRUE, minmax = TRUE, eff = TRUE, eff_na = FALSE,
                         NR = TRUE) {
  if (!moy) ic <- FALSE; sd <- FALSE
  sommaire <- function(var) {
    test <- data %>% select({{var}}) %>% unlist
    nom <- data %>% select({{var}}) %>% names
    if (is.numeric(test)) {
      warning(paste("La variable" , nom, "n'est pas categorielle mais numerique !"), call. = FALSE)
    }
    suppressWarnings(
      tab <- data %>%
        group_by({{var}}) %>%
        summarise(Minimum = min({{var_princ}}, na.rm = TRUE),
                  Maximum = max({{var_princ}}, na.rm = TRUE),
                  Moyenne = mean({{var_princ}}, na.rm = TRUE),
                  `Ecart-type` = round(sd({{var_princ}}, na.rm = TRUE), nb),
                  `IC-` = round(ci({{var_princ}}, alpha = ic_seuil, na.rm = TRUE)[2], nb),
                  `IC+` = round(ci({{var_princ}}, alpha = ic_seuil, na.rm = TRUE)[3], nb),
                  Q1 = quantile({{var_princ}}, probs = 0.25, na.rm = TRUE),
                  Mediane = quantile({{var_princ}}, probs = 0.5, na.rm = TRUE),
                  Q3 = quantile({{var_princ}}, probs = 0.75, na.rm = TRUE),
                  N = sum(!is.na({{var_princ}})),
                  `N avec NR` = n()) %>%
        rename(Modalites = 1) %>%
        mutate(Personne = is.na(Modalites),
               Modalites = as.character(Modalites))
    )
    if (sum(is.na(tab$`IC-`)) > 0) {
      warning(paste("Pas d'ecart-type ou d'intervalle de confiance calcules : la variable" , nom, "comporte au moins une modalite avec une seule reponse pour la variable d'interet."), call. = FALSE)
    }
    if (sum(is.na(tab$Moyenne)) > 0) {
      warning(paste("Pas de moyenne calculee : la variable" , nom, "comporte au moins une modalite avec que des non reponses pour la variable d'interet."), call. = FALSE)
    }
    if (!moy) tab <- tab %>% select(-Moyenne)
    if (!sd) tab <- tab %>% select(-`Ecart-type`)
    if (!ic) tab <- tab %>% select(-c(`IC-`, `IC+`))
    if (!med) tab <- tab %>% select(-Mediane)
    if (!quart) tab <- tab %>% select(-c(Q1, Q3))
    if (!minmax) tab <- tab %>% select(-c(Minimum, Maximum))
    if (!eff) tab <- tab %>% select(-c(N, `N avec NR`))
    if (eff == TRUE & eff_na == FALSE) tab <- tab %>% select(-`N avec NR`)
    if (!NR) tab <- tab %>% filter(!Personne)
        if (sum(is.na(tab$`IC-`)) > 0) {
      warning(paste("Pas d'ecart-type ou d'intervalle de confiance calcules : la variable" , nom, "comporte au moins une modalite avec une seule reponse pour la variable d'interet."), call. = FALSE)
    }
    if (sum(is.na(tab$Moyenne)) > 0) {
      warning(paste("Pas de moyenne calculee : la variable" , nom, "comporte au moins une modalite avec que des non reponses pour la variable d'interet."), call. = FALSE)
    }
    tab %>% select(-Personne)
  }
  list_vars <- rlang::enquos(..., .named = TRUE)
  if (length(list_vars) == 0) {
    stop(paste("Pas de variable a croiser avec", deparse(substitute(var_princ))))
  }
  map_dfr(list_vars, ~sommaire(!!.x), .id = "Variables")
}


