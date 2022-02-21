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
#' @param p.val FALSE par défaut. Si TRUE alors la p-value s'affiche en dernière colonne.
#' @param cram.v FALSE par défaut. Si TRUE alors le V de cramer s'affiche en dernière colonne. Afficher le V de Cramer implique d'afficher la p-value
#' @param sign 0.05 par défaut. Si le test du Khi2 n'est pas significatif au seuil choisi alors pas de calcul du V de Cramer.
#' @param tot Affiche ou non les totaux en ligne et/ou en colonne. c("row", "col") par défaut, peut être aussi "row" ou "col" ou NULL.
#' @param eff TRUE par défaut. affiche les effectifs par case entre parenthèse.
#' @param pourcent TRUE par défaut. Met le signe % après les pourcentages.
#' @param pond variable de pondération (facultatif), doit absolument être écrit sous la forme data$pond
#' @param norm_pond logique. Si TRUE alors normalise les poids de la pondération (la somme des poids est alors égale a la somme des effectifs). FALSE est la valeur par défaut.
#'
#' @return Un tabyl data.frame regroupant tous les tableaux croisés avec pourcentages et effectifs. Si les pourcentages sont en ligne et que les totaux sont activés alors la ligne de total est nommée "Ensemble" et la colonne de total est nommée "Total" et inversement pour les pourcentages en colonne.
#' @export
#'
#' @importFrom stats na.omit as.formula lm update.formula
#' @importFrom rlang enquos
#' @importFrom tibble tibble rownames_to_column
#' @importFrom purrr map_dfr
#' @importFrom dplyr %>% mutate rename if_else select case_when
#' @importFrom questionr wtd.table
#' @importFrom janitor %>% tabyl adorn_totals adorn_percentages adorn_pct_formatting adorn_ns as_tabyl chisq.test

multi_croise <- function(data, var_princ, ..., NR = FALSE, pct_ligne = TRUE, nb = 1, p.val = FALSE,
                         cram.v = FALSE, sign = 0.05, tot = c("row", "col"), eff = TRUE, pourcent = FALSE,
                         pond = NULL, norm_pond = FALSE) {

  if (cram.v) {p.val <- TRUE} # si V de Cramer alors forcement p value
  if (is.null(pond)) {# si pas de ponderation alors simplifie
    tableau <- function(var){
      tab <- data %>%
        tabyl({{var}}, {{var_princ}}, show_na = NR)
    }
  }
  else {# cas avec ponderation
    nr <- ifelse(NR == FALSE, "no", "ifany") # NR pour le wtd.table

    var_princ2 <- data %>% select({{var_princ}}) %>% unlist(use.names = FALSE) # on vectorise

    tableau <- function(var){ # fonction plus haut adaptee au wtd.table
      var2 <- data %>% select({{var}}) %>% unlist(use.names = FALSE) # idem

      tab <- wtd.table(var2, var_princ2, weights = pond, normwt = norm_pond, useNA = nr) %>%
        as.data.frame.array %>%
        round(0) %>% # arrondi de la ponderation
        rownames_to_column() %>% # modalites
        as_tabyl() # on retombe sur le meme cas que plus haut donc idem
    }
  }

  transformation <- function(tabl){
    if (p.val) {
      p <- janitor::chisq.test(tabl)$p.value
      if (p > sign) {cram.v <- FALSE}
      p <- format(p, digits = 2, scientific = ifelse(p < 0.001, T, F))
      }
    if (cram.v) {
      n <- sum(tabl[2:ncol(tabl)])
      chid <- janitor::chisq.test(tabl, correct = FALSE)$statistic
      dim <- min(nrow(tabl), ncol(tabl)) - 1
      v <- as.numeric(sqrt(chid/(n * dim))) %>% format(digits = 2) %>% paste("V =", .)
      }

    tab <- tabl %>%
      adorn_totals(where = tot, # les totaux a mettre
                   name = case_when(pct_ligne ~ c("Ensemble", "Total"),
                                    TRUE      ~ c("Total", "Ensemble"))) %>%
      # les noms des totaux selon si pct en ligne ou en colonne
      adorn_percentages(denominator = ifelse(pct_ligne, "row", "col")) %>%
      adorn_pct_formatting(digits = nb, rounding = "half up", affix_sign = pourcent) #decimales

    if (eff) {tab <- tab %>% adorn_ns()} # effectifs sous la forme (n)
    if (p.val) {
      if (cram.v) {tab <- tab %>% mutate(Khi2 = c(p, rep(v, nrow(tab) - 1)))}
      else {tab <- tab %>% mutate(Khi2 = rep(p, nrow(tab)))}
      }
    # tabyl est un data.frame donc on peut mutate

    tab %>% rename(Modalites = 1) # pour pouvoir bind
  }

  # on cree une liste de toutes les variables a utiliser dans le tableau
  list_vars <- rlang::enquos(..., .named = TRUE)
  map_dfr(list_vars, ~ transformation(tableau(!!.x)), .id = "Variables")
  # map_dfr applique la fonction tableau() a chaque element de la liste
  # a chaque tableau produit, le nom des variables est ajoute sur la gauche
}
