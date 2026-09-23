#' @title map_quanti
#' @author Julio Ricardo Davalos
#'
#' @description
#' Applique [multi_quanti()] à plusieurs variables quantitatives et catégorielles.
#'
#' @param data Base de données. Peut être omise si un `design` valide  est fourni.
#' @param ... Variables quantitatives et catégorielles à analyser.
#' @param moy `TRUE` par défaut. Calcule la moyenne par modalité.
#' @param test.diffmoy `TRUE` par défaut. Réalise un test global d'égalité des moyennes entre les modalités, tenant compte du design avec `survey::svyglm()` et `survey::regTermTest()`.
#'   Ce test repose sur une approximation asymptotique et doit être interprété avec prudence lorsque les effectifs ou le nombre de grappes sont faibles, ou lorsque la distribution comporte des valeurs extrêmes.
#' @param sd `TRUE` par défaut. Calcule l'écart-type par modalité.
#' @param ic `TRUE` par défaut. Calcule l'intervalle de confiance de la moyenne par modalité. Ignoré si `moy = FALSE`.
#' @param ic_seuil Risque de première espèce utilisé pour l'intervalle de confiance des moyennes.
#' @param nb Nombre de décimales pour les statistiques calculées.
#' @param med `TRUE` par défaut. Calcule la médiane par modalité.
#' @param quant `4` par défaut. Nombre de quantiles. Si la médiane est sélectionnée, elle est ajoutée si nécessaire.
#' Utiliser `1` ou moins pour ne pas calculer les quantiles.
#' @param minmax `TRUE` par défaut. Calcule le minimum et le maximum par modalité.
#' @param eff `TRUE` par défaut. Calcule les effectifs pondérés par modalité.
#' @param freq `TRUE` par défaut. Calcule les fréquences pondérées par modalité.
#' @param eff_na `FALSE` par défaut. Calcule les effectifs pondérés des non-réponses de la variable quantitative par modalité.
#' @param signif `NULL` par défaut. Peut prendre les valeurs `"etoiles"`ou `"seuils"` pour remplacer la p-value par des étoiles ou des seuils.
#' @param pond Variable de pondération facultative, à fournir sans guillemets. Utilisée si aucun `design` valide n'est fourni.
#' @param design Objet créé avec `survey::svydesign()` ou `survey::svrepdesign()`.
#' @param norm_pond `FALSE` par défaut.
#' Normalise les poids utilisés pour les effectifs affichés afin que leur somme corresponde au nombre d'observations conservées.
#' Cette normalisation ne modifie pas le plan utilisé pour les estimations et les tests.
#' @param NR `FALSE` par défaut. Conserve les non-réponses des variables catégorielles.
#' @param msg `FALSE` par défaut. Affiche un message à la fin du traitement de chaque variable.
#'
#' @return Un tibble contenant les indicateurs synthétiques de chaque variable quantitative selon les modalités de chaque variable catégorielle.
#'
#' @export
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr select rename
#' @importFrom tidyselect where
#'
map_quanti <- function(data = NULL, ...,
                       moy = TRUE, test.diffmoy = TRUE, sd = TRUE, ic = TRUE, ic_seuil = 0.05, nb = 2,
                       med = TRUE, quant = 4, minmax = TRUE, eff = TRUE, eff_na = FALSE, freq = TRUE, signif = NULL,
                       pond = NULL, design = NULL, norm_pond = TRUE, NR = FALSE, msg = FALSE) {
  # Creation ou validation du design une seule fois
  design <- creer_design(data = data, pond = {{pond}}, design = design)

  # Les donnees contenues dans le design deviennent la reference
  data <- design$variables

  # Variables selectionnees par l'utilisateur
  data_vars <- data |> select(...)

  # Variables quantitatives
  data_vars_num <- data_vars |> select(where(is.numeric))

  # Variables categorielles
  data_vars_cat <- data_vars |> select(where(~ !is.numeric(.x)))

  list_vars_num <- map(rlang::set_names(names(data_vars_num)), ~ rlang::quo(!!as.name(.x)))

  list_vars_cat <- map(rlang::set_names(names(data_vars_cat)), ~ rlang::quo(!!as.name(.x)))

  if (length(list_vars_num) == 0) {
    stop("Pas de variable numerique selectionnee.", call. = FALSE)
  }

  if (length(list_vars_cat) == 0) {
    stop("Pas de variable categorielle selectionnee.", call. = FALSE)
  }

  # Application de multi_quanti a chaque combinaison
  map(list_vars_num, function(x) {
    map(list_vars_cat, function(y) {
      multi_quanti(data = NULL, var_princ = !!x, !!y,
                   moy = moy, test.diffmoy = test.diffmoy, sd = sd, ic = ic, ic_seuil = ic_seuil, nb = nb,
                   med = med, quant = quant, minmax = minmax, eff = eff, eff_na = eff_na, freq = freq, signif = signif,
                   design = design, norm_pond = norm_pond, NR = NR, msg = msg)
        }) |>
      list_rbind() |>
      rename(`Variable categorielle` = Variable)
    }
  ) |>
    list_rbind(names_to = "Variable quantitative")
}
