#' @title separe_ind_afm
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'extraire la liste des analyses séparées des individus d'une analyse factorielle multiple.
#'
#' @param afm objet MFA
#' @param nb nombre de chiffres à conserver pour toutes les statistiques produites.
#' @param act_uniq FALSE par défaut. Doit- extraire seulement les analyses factorielles actives ?
#'
#' @return Un data.frame comprenant le nom des variables, modalités, si elles sont actives ou supplémentaires ainsi que l'ensemble des données générées par les AGD de l'AFM (coordonnées, cos2, contributiosn etc.).
#' @export
#'
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate across count add_count filter select case_when distinct cur_column relocate
#' @importFrom tidyselect everything matches

separe_ind_afm <- function(afm, nb = 3, act_uniq = FALSE) {
  if (class(afm)[1] != "MFA") {stop("Pas objet AFM")}
  groupes <- c(0, cumsum(afm$call$group))
  num_groupes <- 1:length(afm$separate.analyses)

  if (act_uniq) {
    num_groupes <- num_groupes[-afm$call$num.group.sup]
  }

  if (exists("aux.base", envir = globalenv())) {
    assign("baseaunomimpossiblegenrevraiment123456789", aux.base, envir = globalenv())
  }

  aux.base <- assign("aux.base", get(afm$call$call$base), envir = globalenv())
  res <- map_dfr(setNames(num_groupes, names(afm$separate.analyses)[num_groupes]),
                 ~qessmasteR::tab_ind_agd(afm$separate.analyses[[.x]], nb = nb), .id = "groupe")
  rm(aux.base, envir = globalenv())

  if (exists("baseaunomimpossiblegenrevraiment123456789", envir = globalenv())) {
    assign("aux.base", baseaunomimpossiblegenrevraiment123456789, envir = globalenv())
    rm(baseaunomimpossiblegenrevraiment123456789, envir = globalenv())
  }

  res %>%
    relocate(groupe, .after = type) %>%
    arrange(as.numeric(ID))
}
