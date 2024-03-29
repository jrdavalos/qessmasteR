#' @title lprop_pctot
#' @author Julio Ricardo Davalos
#' @description Permet d'obtenir un tableau croise avec les pourcentages en lignes, mais ajoute une colonne avec le pourcentage total de la modalité.
#' En cas de variable de pondération, les pourcentages en lignes et de la modalité sont pondérés mais on peut choisir entre effectifs pondérés et effectifs réels.
#'
#' @param data les données
#' @param x variable a mettre en ligne
#' @param y variable a mettre en colonne
#' @param pond variable de pondération (facultatif), doit absolument être écrit sous la forme data$pond
#' @param tot_pond logique. Si FALSE (valeur par deéfaut) alors les effectifs ne sont pas pondérés.
#' @param norm_pond TRUE par défaut. Si TRUE alors normalise les poids de la pondération (la somme des poids est alors égale a la somme des effectifs).
#' @param num logique. Si TRUE (valeur par défaut) alors les effectifs sont affiches
#' @param ch nombre de chiffres apres la virgule
#' @param nr comportements vis-a-vis des NA (voir table())
#'
#' @return un data.frame
#' @importFrom questionr lprop wtd.table
#' @importFrom dplyr select
#' @importFrom magrittr %>% %$% %<>%
#'
#' @export
#' @examples
#' data <- data.frame(id=1:500, group = rep(LETTERS[1:2], 500/2), age = sample(18:30, 500, replace = TRUE), weights = abs(rnorm(500)))
#' lprop_pctot(data, age, group, data$weights)
lprop_pctot <- function(data, x, y, pond = NULL, tot_pond = FALSE, norm_pond = FALSE, num = TRUE, ch = 1, nr = c("no", "ifany", "always")){
  # tableau des pct par ligne
  if (nr[1] == "always") {d = FALSE} else d = TRUE
  if (is.null(pond)) {# si pas de ponderation alors tableau des effectifs
    tab <- data %>%
      select({{x}},{{y}}) %>%
      table(useNA = nr) %>%
      lprop(digits = ch, n = TRUE, drop = d) # oblige de mettre n = TRUE pour calcul des pct
  }
  else {# si ponderation alors wtd.table
    tab <- wtd.table(data[, which(colnames(data) == deparse(substitute(x)))] %>% unlist(use.names = FALSE),
                     data[, which(colnames(data) == deparse(substitute(y)))] %>% unlist(use.names = FALSE),
                     weights = pond, normwt = norm_pond, useNA = nr) %>%
      lprop(digits = ch, n = TRUE, drop = d)
  }
  # poids des lignes (pct total)
  lignes <- nrow(tab)
  colonnes <- ncol(tab)
  colnames(tab)[colonnes - 1] <- "% total"
  for (i in 1:lignes){
    tab[i, colonnes - 1] <- 100 * tab[i, colonnes] / tab[lignes, colonnes]
  }
  tab <- as.data.frame.array(round(tab, ch)) # pour pouvoir arrondir
  # prise en compte des effectifs par ligne ou non
  if (num == TRUE) {
    if (tot_pond == FALSE) {# si pas effectifs non ponderes
      colnames(tab)[colonnes] <- "Eff. reels"
      tot_non_pond <- data %>%
        select({{x}},{{y}}) %>%
        table(useNA = nr) %>%
        lprop(n = TRUE, digits = ch, drop = d) %>%
        as.data.frame.array() %>% # pour l'arrondi a l'unite
        select(n) %>%
        unlist(use.names = FALSE)
      tab[colonnes] <- tot_non_pond
    }
    else {
      colnames(tab)[colonnes] <- "Eff. pond."
      tab[,colonnes] <- round(tab[,colonnes])
    }
    tab[,1:(colonnes - 1)] <- round(tab[,1:(colonnes - 1)], ch)
    tab[,colonnes] <- round(tab[,colonnes])
    tab <- tab[, c(1:(colonnes - 2), colonnes, colonnes - 1)]
  }
  if (num == FALSE) {
    tab <- round(tab[,-colonnes], ch)
  }
  return(tab)
}
