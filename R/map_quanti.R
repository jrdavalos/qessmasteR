#' @title map_quanti
#' @author  Julio Ricardo Davalos
#'
#' @description Applique multi_quanti à plusieurs variables quantitatives
#'
#' @param data base de données
#' @param ... variables catégorielles et quantitatives
#' @param moy TRUE par défaut. Moyenne par modalité.
#' @param test.diffmoy TRUE par défaut. Réalise un ANOVA et retourne la p-value du test d'égalité des moyennes adapté.
#' La procédure est la suivante: on réalise une ANOVA et un test de Shapiro-Wilk (test de normalité, remplacé par un test de Kolmogorov-Smirnov si l'effectif est supérieur à 5000 étant donné que le test de Shapiro-Wilk est sensible aux grands effectifs) sur celle-ci.
#' Si la normalité des résidus est rejetée alors le résultat final est un test de Kruskal-Wallis (on aura alors P(K)).
#' Si elle ne l'est pas alors on réalise un test de Levene (homoscédasticité).
#' Si l'homoscédasticité est rejetée alors le résultat final est un ANOVA de Welch (on aura alors P(W)), sinon on conserve l'ANOVA (on aura alors P(A)).
#' @param force_anova Permet de forcer la fonction à considérer la distribution comme normale. Vecteur des noms des variables catégorielles pour lesquelles forcer.
#' Peut être utile si on considère que la distribution est malgré tout suffisamment proche d'une distribution normale pour ne pas avoir d'incidence sur l'analyse.
#' Peut prendre la valeur TRUE si on veut forcer pour toutes les variables.
#' @param ic_test risque de première espèce des tests de d'égalité des moyennes.
#' @param sd TRUE par défaut. Écart-type par modalité.
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne par modalité. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param nb nombre de décimales pour la moyenne, l'écart-type et l'intervalle de confiance.
#' @param med TRUE par défaut. Médiane par modalité.
#' @param quant 4 par défaut. Nombre de quantiles. Si la médiane est sélectionnée, elle sera ajoutée si besoin.
#' @param minmax TRUE par défaut. Minimum et maximum par modalité.
#' @param eff TRUE par défaut. Effectifs par modalité.
#' @param freq TRUE par défaut. Fréquence par modalité.
#' @param eff_na FALSE par défaut. Effectifs des non-réponses dans la variable quantitative par modalité.
#' @param NR FALSE par défaut. Garde les non-réponses des variables catégorielles.
#' @param msg FALSE par défaut. Envoie un message pour chaque variable terminée : utile si bug inexpliqué.
#' @param signif NULL par défaut, prend les valeurs 'etoiles' ou 'seuils'. Permet d'afficher les résultats des tests avec des étoiles ou avec des seuils à la place de la p.value en clair.
#'
#' @return Un tibble avec en colonne les indicateurs synthétiques de la variable d'intérêt selon les modalités des variables catégorielles choisies.
#' @export
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr where

map_quanti <- function(data, ..., moy = TRUE, test.diffmoy = TRUE, force_anova = NULL, ic_test = 0.05, sd = TRUE, ic = TRUE,
                       ic_seuil = 0.05, nb = 2, med = TRUE, quant = 4, minmax = TRUE, eff = TRUE, eff_na = FALSE, freq = TRUE,
                       signif = NULL, NR = FALSE, msg = FALSE) {
  # d'abord le dataframe avec uniquement les variables souhaitées :
  data_vars_num <- data %>% select(...) %>% select(where(is.numeric))
  data_vars_cat <- data %>% select(...) %>% select(where(is.character))
  # on peut faire la liste des NOMS comme suit :
  list_vars_num <- map(set_names(names(data_vars_num)), ~ quo(!!as.name(.x)))
  list_vars_cat <- map(set_names(names(data_vars_cat)), ~ quo(!!as.name(.x)))
  if (length(list_vars_num) == 0) {
    stop(paste("Pas de variable numerique !"))
  }
  if (length(list_vars_cat) == 0) {
    stop(paste("Pas de variable categorielle !"))
  }
  # on applique multi_quanti à la liste :
  map(list_vars_num,
      ~multi_quanti(data, .x, list_vars_cat,
                    moy = moy, test.diffmoy = test.diffmoy, force_anova = force_anova, ic_test = ic_test, sd = sd, ic = ic,
                    ic_seuil = ic_seuil, nb = nb, med = med, quant = quant, minmax = minmax, eff = eff, eff_na = eff_na,
                    freq = freq, signif = signif, NR = NR, msg = msg)) %>%
    list_rbind()

}
