#' @title  desc_quanti
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir les principaux indicateurs concernant un groupe de variables quantitatives
#'
#' @param data base de données
#' @param ... variables souhaitées
#' @param moy TRUE par défaut. Moyenne de l'échantillon. Si FALSE, alors pas non plus d'écart-type.
#' @param sd TRUE par défaut. Ecart-type de l'échantillon
#' @param test.norm TRUE par défaut. P-value du test de normalité de l'échantillon (test de Shapiro-Wilk).
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param nb 1 par défaut. Nombre de décimales.
#' @param med TRUE par défaut. Médiane de l'échantillon
#' @param quart TRUE par défaut. Quartiles Q1 et Q3 de l'échantillon.
#' @param minmax TRUE par défaut. Minimum et maximum .
#' @param eff TRUE par défaut. Effectifs.
#' @param eff_na FALSE par défaut. Remplace les effectifs par les effectifs avec les NA. N'apparait pas si eff = FALSE
#' @param NR TRUE par défaut. Supprime les non-réponses.
#'
#' @return un data.frame avec les indicateurs sélectionnés pour les variables quantitatives choisies
#'
#' @importFrom dplyr bind_rows mutate %>% filter group_by count rename select pull across summarise
#' @importFrom tidyselect everything
#' @importFrom purrr map map_dfr map_lgl keep
#' @importFrom gmodels ci
#'
#' @export

desc_quanti <- function(data, ..., moy = TRUE, sd = TRUE, test.norm = TRUE, ic = TRUE, ic_seuil = 0.05, nb = 2, med = TRUE,
                       quart = TRUE, minmax = TRUE, eff = TRUE, eff_na = FALSE, NR = TRUE) {
  if (!moy) {
    ic <- FALSE
    sd <- FALSE
  }
  # tidyselect des variables a utiliser :
  data_vars <- data %>% select(...)
  if (ncol(data_vars) == 0) {
    warning("Pas de variable selectionnee, utilisation de toutes les variables numeriques.", call. = FALSE)
    data_vars <- data %>% select(everything() & where(is.numeric))
  }
  if (!(data_vars %>% every(is.numeric))) {
    data_vars <- data_vars %>% keep(is.numeric)
    warning(paste0("Variable(s) ignoree(s) car non-numerique(s) : ", paste(names(list_num)[which(!list_num)], collapse = ", ")))
  }
  # liste des noms :
  list_vars <- map(set_names(names(data_vars)), ~ quo(!!as.name(.x)))

  ######### ELEMENT A GERER PLUS TARD ##########
  # gestion de la ponderation (si vecteur ou non + pb de longueur)
  # if (!is.null(pond)) {# si pas de ponderation alors pas besoin de la normaliser
  #   if (nrow(data_vars) != length(pond)) {
  #     stop("Le vecteur de ponderation n'est pas de la bonne longueur.")
  #   }
  # } else norm_pond <- FALSE

  # fonction de tri a plat par variable
  desc <- function(var) {
    # on selectionne la variable
    tab <- data %>% select({{var}})
    nom <- names(tab)

    ######### ELEMENT A GERER PLUS TARD ##########
    # si ponderation on ajoute au nouveau dataframe :
    # if (!is.null(pond)) {
      # on ajoute le vecteur de poids
    #   tab <- tab %>% mutate(pond = pond)
    # }

    if (!NR) {
      tab1 <- tab
      # on enleve les NA
      tab <- tab %>% filter(!is.na({{var}}))
        # s'il n'y a que des NA alors pas de lignes
      if (nrow(tab) == 0) {
        warning(paste(nom, " ne contient que des non-reponses, elles sont gardees pour cette variable."), call. = FALSE)
        tab <- tab1
      }
      ######### ELEMENT A GERER PLUS TARD ##########
      # normalisation de la ponderation si besoin et si demande
      # if (norm_pond & sum(pond, na.rm = T) != nrow(tab)) {
      #   tab <- tab %>% mutate(pond = pond / mean(pond, na.rm = T))
      # }
    }
    suppressWarnings(
      tab <- tab %>%
        summarise(Minimum = min({{var}}, na.rm = TRUE),
                  Maximum = max({{var}}, na.rm = TRUE),
                  Moyenne = round(mean({{var}}, na.rm = TRUE), nb),
                  `IC-` = round(ci({{var}}, alpha = ic_seuil, na.rm = TRUE)[2], nb),
                  `IC+` = round(ci({{var}}, alpha = ic_seuil, na.rm = TRUE)[3], nb),
                  `Ecart-type` = round(sd({{var}}, na.rm = TRUE), nb),
                  `Shapiro-Wilk` =  format(shapiro.test({{var}})$p.value, digits = 2,
                                           scientific = ifelse(shapiro.test({{var}})$p.value < 0.001, T, F)),
                  Q1 = quantile({{var}}, probs = 0.25, na.rm = TRUE),
                  Mediane = quantile({{var}}, probs = 0.5, na.rm = TRUE),# idee : probs = seq(0, 1, 1/n) => summarise(across())
                  Q3 = quantile({{var}}, probs = 0.75, na.rm = TRUE),
                  N = sum(!is.na({{var}})),
                  `N avec NR` = n())
    )
    if (!sd) tab <- tab %>% select(-`Ecart-type`)
    if (!ic) tab <- tab %>% select(-c(`IC-`, `IC+`))
    if (!moy) tab <- tab %>% select(-Moyenne)
    if (!med) tab <- tab %>% select(-Mediane)
    if (!quart) tab <- tab %>% select(-c(Q1, Q3))
    if (!minmax) tab <- tab %>% select(-c(Minimum, Maximum))
    if (!eff) tab <- tab %>% select(-c(N, `N avec NR`))
    if (eff == TRUE & eff_na == FALSE) tab <- tab %>% select(-`N avec NR`)
    return(tab)
    }

  # boucle pour toute la liste :
  map_dfr(list_vars, ~desc(!!.x), .id = "Variable")
}
