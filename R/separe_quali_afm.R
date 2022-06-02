#' @title separe_quali_afm
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'extraire la liste des analyses séparées d'une analyse factorielle multiple.
#'
#' @param afm objet MFA
#' @param sup_dbl TRUE par défaut. La fonction colle le nom de la variable à la modalité en cas de doublon. Ici, on supprime cette concaténation pour plus de lisibilité dans le tableau final. Peut gêner la lecture dans le cas d'une représentation graphique basée uniquement sur les modalités.
#' @param nb nombre de chiffres à conserver pour toutes les statistiques produites.
#' @param act_uniq FALSE par défaut. Est-ce qu'on doit extraire seulement les analyses factorielles actives ?
#'
#' @return Un data.frame comprenant le nom des variables, modalités, si elles sont actives ou supplémentaires ainsi que l'ensemble des données générées par l'AGD (coordonnées, cos2, contributiosn etc.).
#' @export
#'
#' @importFrom purrr map

separe_quali_afm <- function(afm, sup_dbl = TRUE, nb = 3, act_uniq = FALSE) {
  if (class(afm)[1] != "MFA") {stop("Pas objet AFM")}
  base <- get(afm$call$call$base)
  groupes <- c(0, cumsum(afm$call$group))

  extraire <- function(groupe){
    vars_index <- groupes[groupe]:groupes[groupe + 1]
    assign("aux.base", base[, vars_index], envir = globalenv())
    return(qessmasteR::tab_quali_agd(afm$separate.analyses[[groupe]], sup_dbl = sup_dbl, nb = nb))
  }

  num_groupes <- 1:length(afm$separate.analyses)

  if (act_uniq) {
    num_groupes <- num_groupes[-afm$call$num.group.sup]
  }

  res <- map(num_groupes, ~extraire(.x))
  names(res) <- names(afm$separate.analyses)[num_groupes]
  rm(aux.base, envir = globalenv())

  return(res)
}
