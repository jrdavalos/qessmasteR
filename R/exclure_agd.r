#' @title exclure_agd
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir l'objet regroupant les indexes des modalités à exclure des différents objets d'analyse géométrique des données. Par défaut, on exclue les non-réponses, mais on peut ajouter d'autres cas.
#'
#' @param agd objet AGD (MCA, MFA, PCA...)
#' @param na indexe tous les NA (par défaut, TRUE)
#' @param modalites NULL par défaut. Sinon, liste de vecteurs nommés du nom de la variable et contenant la ou les modalités considérées
#'
#' @return vecteur ou liste des indexes, selon l'objet de départ
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map map_dbl map2
#' @importFrom dplyr %>% summarise count case_when
#' @importFrom stringr str_detect

exclure_agd <- function(agd, na = TRUE, modalites = NULL) {
  if (!na & is.null(modalites)) {
    message("Pas de modalites a exclure.")
    return(NULL)
  }
  
  type <- class(agd)[1]
  
  if (type == "MCA") {
    acm <- do.call(MCA, list(X = agd$call$call$X, graph = F, ncp = 1))
    noms <- colnames(acm$call$Xtot)
    base <- get(a$call$call$X)
    nombres <- 1:length(noms)
  } else if (type == "PCA") {
    message("Pas encore finalise pour les ACP.")
  } else if (type == "MFA") {
    noms <- rownames(agd$call$quali.sup$barycentre)
    base <- get(agd$call$call$base)
    nombres <- map(agd$call$group.mod, ~ seq(1, .x, by = 1)) %>% unlist()
  } else {message("Pas objet AGD !")}
  
  if (!is.null(modalites)) {
    var_unique <- base %>% 
      pivot_longer(everything(), names_to = "variables", values_to = "modalites") %>% 
      group_by(variables, modalites) %>% 
      summarise() %>% 
      ungroup() %>% 
      add_count(modalites) %>% 
      filter(n == 1) %>% 
      select(modalites) %>% 
      distinct() %>% 
      unlist()
    var <- rep(names(modalites), map_dbl(names(modalites), ~length(modalites[[.x]])))
    modalites <- case_when(unlist(modalites) %in% var_unique ~ unlist(modalites),
                           TRUE ~ case_when(unlist(modalites) == "NA" | is.na(modalites) ~ paste(var, unlist(modalites), sep = "."),
                                            TRUE ~ paste(var, unlist(modalites), sep = "_")))
  }
  
  liste_modalites <- noms %in% modalites
  
  if (na) {
    liste_na <- str_detect(noms, "NA") & !(noms %in% modalites)
  } else liste_na <- rep(FALSE, length(noms))
  
  index <- liste_na * nombres + liste_modalites * nombres
  
  if (type == "MCA") {return(index[index != 0])}
  if (type == "MFA") {
    index_group <- (map2(agd$call$group.mod, 1:length(agd$call$group.mod), 
                         ~ rep(.y, .x)) %>% 
                      unlist()) * (index != 0)
    
    split(index, index_group)[-1] %>% `names<-`(agd$call$name.group)
  }
}
