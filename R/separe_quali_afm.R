#' @title separe_quali_afm
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'extraire les analyses séparées des variables d'une analyse factorielle multiple.
#'
#' @param afm objet MFA
#' @param sup_dbl TRUE par défaut. La fonction colle le nom de la variable à la modalité en cas de doublon. Ici, on supprime cette concaténation pour plus de lisibilité dans le tableau final. Peut gêner la lecture dans le cas d'une représentation graphique basée uniquement sur les modalités.
#' @param nb nombre de chiffres à conserver pour toutes les statistiques produites.
#' @param act_uniq FALSE par défaut. Doit-on extraire seulement les analyses factorielles actives ?
#'
#' @return Un data.frame comprenant le nom des variables, modalités, si elles sont actives ou supplémentaires ainsi que l'ensemble des données générées par les AGD de l'AFM (coordonnées, cos2, contributiosn etc.).
#' @export
#'
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate across count add_count filter select case_when distinct cur_column
#' @importFrom tidyselect everything matches

separe_quali_afm <- function(afm, sup_dbl = TRUE, nb = Inf, act_uniq = FALSE) {
  if (class(afm)[1] != "MFA") {stop("Pas objet AFM")}
  base <- get(afm$call$call$base)
  var_reco <- base %>% # on doit savoir quelles modalites vont apparaitre avec leur variable
    pivot_longer(everything(), names_to = "variable", values_to = "modalite") %>% # prend les modalite s
    count(variable, modalite) %>% # compte
    add_count(modalite, name = "nn") %>%
    filter(nn > 1) %>%
    select(variable) %>%
    distinct() %>%
    unlist(use.names = F)
  groupes <- c(0, cumsum(afm$call$group))

  extraire <- function(groupe) {
    vars_index <- (1 + groupes[groupe]):groupes[groupe + 1]# on prend la 1ere et la derniere modalite du groupe
    assign("aux.base", base[, vars_index] %>% # on recode pour avoir toutes les modalites comme il faut
             mutate(across(matches(var_reco), ~ case_when(!is.na(.x) ~ paste(cur_column(), .x, sep = "_"),
                                                          TRUE ~ paste0(cur_column(), ".NA")))), envir = globalenv())
    return(qessmasteR::tab_quali_agd(afm$separate.analyses[[groupe]], sup_dbl = sup_dbl, nb = nb))
  }

  num_groupes <- 1:length(afm$separate.analyses)

  if (act_uniq) {
    num_groupes <- num_groupes[-afm$call$num.group.sup]
  }
  if (exists("aux.base", envir = globalenv())) {assign("baseaunomimpossiblegenrevraiment123456789", aux.base, envir = globalenv())}
  res <- map_dfr(setNames(num_groupes, names(afm$separate.analyses)[num_groupes]), ~extraire(.x), .id = "groupe")
  rm(aux.base, envir = globalenv())
  if (exists("baseaunomimpossiblegenrevraiment123456789", envir = globalenv())) {
    assign("aux.base", baseaunomimpossiblegenrevraiment123456789, envir = globalenv())
    rm(baseaunomimpossiblegenrevraiment123456789, envir = globalenv())}
  return(res)
}
