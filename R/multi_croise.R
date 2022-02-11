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
#' @param nb nombre de décimales pour les pourcentages. 1 par défaut
#' @param pond variable de pondération (facultatif), doit absolument être écrit sous la forme data$pond
#' @param tot_pond logique. Si FALSE (valeur par deéfaut) alors les effectifs ne sont pas pondérés.
#' @param norm_pond logique. Si TRUE alors normalise les poids de la pondération (la somme des poids est alors égale a la somme des effectifs). FALSE est la valeur par défaut.
#'
#' @return un tibble regroupant tous les tableaux croisés avec pourcentages et effectifs.
#' @export
#'
#' @importFrom stats na.omit as.formula lm update.formula
#' @importFrom rlang enquos
#' @importFrom tibble tibble rownames_to_column
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>% mutate rename if_else select
#' @importFrom questionr wtd.table
#' @importFrom janitor %>% tabyl adorn_totals adorn_percentages adorn_pct_formatting adorn_ns as_tabyl

multi_croise <- function(data, var_princ, ..., NR = FALSE, pct_ligne = TRUE, nb = 1,
                         pond = NULL, tot_pond = FALSE, norm_pond = FALSE) {

  if (is.null(pond)) {# si pas de ponderation alors simplifie
    tableau <- function(var){
      data %>%
        tabyl({{var}}, {{var_princ}}, show_na = NR) %>%
        adorn_totals(c("row", "col")) %>% # total en fonction du sens pct
        adorn_percentages(ifelse(pct_ligne, "row", "col")) %>%
        adorn_pct_formatting(digits = nb) %>% # decimales
        adorn_ns() %>% # pct (n)
        tibble %>% # pour pouvoir bind et renommer
        rename(Modalites = 1) %>%
        {if (pct_ligne) mutate(., Modalites = if_else(Modalites == "Total",
                                                      "Ensemble",
                                                      Modalites))
          # si pct en ligne alors les lignes "total" deviennent "ensemble"
          # sinon, c'est la colonne "total" qui le devient
          else rename(., Ensemble = Total)}
      }
    }
  else { # cas avec ponderation
    nr <- ifelse(NR == FALSE, "no", "ifany") # NR pour le wtd.table

    var_princ2 <- data %>% select({{var_princ}}) %>% unlist(use.names = FALSE) # on vectorise

    tableau <- function(var){ # fonction plus haut adaptee au wtd.table
      var2 <- data %>% select({{var}}) %>% unlist(use.names = FALSE) # idem

      wtd.table(var2, var_princ2, weights = pond, normwt = norm_pond, useNA = nr) %>%
        as.data.frame.array %>%
        round(0) %>% # arrondi de la ponderation
        rownames_to_column() %>% # modalites
        as_tabyl() %>% # on retombe sur le meme cas que plus haut
        adorn_totals(c("row", "col")) %>%
        adorn_percentages(ifelse(pct_ligne, "row", "col")) %>%
        adorn_pct_formatting(digits = nb) %>%
        adorn_ns() %>%
        tibble %>%
        rename(Modalites = 1) %>%
        {if (pct_ligne) mutate(., Modalites = if_else(Modalites == "Total",
                                                      "Ensemble",
                                                      Modalites))
          else rename(., Ensemble = Total)}
      }
    }
  # on cree une liste de toutes les variables a utiliser dans le tableau
  list_vars <- rlang::enquos(..., .named = TRUE)
  map_dfr(list_vars, ~ tableau(!!.x), .id = "Variables")
  # map_dfr applique la fonction tableau() a chaque element de la liste
  # a chaque tableau produit, le nom des variables est ajoute sur la gauche
}
