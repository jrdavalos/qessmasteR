#' @title tab_ind_agd
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau contenant toutes les données des individus utilisés dans une analyse géométrique des données.
#'
#' @param agd objet AGD (MCA, MFA, PCA...)
#' @param nb nombre de chiffres à conserver pour toutes les statistiques produites.
#'
#' @return Un data.frame comprenant les coordonnées, contribbutions, cos2 (et inertie intra individuelle pour les AFM) des individus et les modalités de ces individus
#' @export
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr %>% select mutate bind_rows rename_all right_join relocate across
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_extract

tab_ind_agd <- function(agd, nb = 3) {# creation de tous les tableaux pour les acm
  type <- class(agd)[1]

  if (type == "MCA") {
    donnees <- get(agd$call$call$X) %>% rownames_to_column("ID")
  } else if (type == "MFA") {
    donnees <- get(agd$call$call$base) %>% rownames_to_column("ID")
  } else {message("Pas objet AGD ou type non encore finalise !")}

  coordonnees <- as.data.frame(agd$ind$coord) %>%
    rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord")) %>%
    rownames_to_column("ID")

  contributions <- as.data.frame(agd$ind$contrib) %>%
    rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_contrib")) %>%
    rownames_to_column("ID")

  cos2 <- as.data.frame(agd$ind$cos2) %>%
    rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_cos2")) %>%
    rownames_to_column("ID")

  ind_actifs <- donnees %>%
    right_join(coordonnees, by = "ID") %>%
    right_join(contributions, by = "ID") %>%
    right_join(cos2, by = "ID")

  if (type == "MFA") {
    inertie <- as.data.frame(agd$ind$within.inertia) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_inertie")) %>%
      rownames_to_column("ID")

    coord_part <- as.data.frame(agd$ind$coord.partiel) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord_part")) %>%
      rownames_to_column("ID") %>%
      mutate(Groupe = str_split_fixed(ID, "\\.", 2)[,2],
             ID = str_split_fixed(ID, "\\.", 2)[,1])
    noms_groupes <- agd$call$name.group[-agd$call$num.group.sup]
    coord_part <- map_dfc(1:length(noms_groupes), ~coord_part %>%
                            filter(Groupe == noms_groupes[.x]) %>%
                            `colnames<-`(paste(colnames(.), .x, sep = "_"))) %>%
      rename(ID = ID_1) %>%
      select(ID, contains(paste0("_coord_part_", 1:length(noms_groupes))))

    ind_actifs <- ind_actifs %>%
      right_join(inertie, by = "ID") %>%
      right_join(coord_part, by = "ID")
  }

  ind_actifs <- ind_actifs %>% mutate(type = "Actifs")

  # variable sup
  coordonnees_sup <- as.data.frame(agd$ind.sup$coord) %>%
    rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord")) %>%
    rownames_to_column("ID")

  cos2_sup <- as.data.frame(agd$ind.sup$cos2) %>%
    rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_cos2")) %>%
    rownames_to_column("ID")

  ind_sup <- donnees %>%
    right_join(coordonnees_sup, by = "ID") %>%
    right_join(cos2_sup, by = "ID")

  if (type == "MFA") {
    inertie_sup <- as.data.frame(agd$ind.sup$within.inertia) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_inertie")) %>%
      rownames_to_column("ID")

    coord_part_sup <- as.data.frame(agd$ind.sup$coord.partiel) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord_part")) %>%
      rownames_to_column("ID") %>%
      mutate(Groupe = str_split_fixed(ID, "\\.", 2)[,2],
             ID = str_split_fixed(ID, "\\.", 2)[,1])
    coord_part_sup <- map_dfc(1:length(noms_groupes), ~coord_part_sup %>%
                                filter(Groupe == noms_groupes[.x]) %>%
                                `colnames<-`(paste(colnames(.), .x, sep = "_"))) %>%
      rename(ID = ID_1) %>%
      select(ID, contains(paste0("_coord_part_", 1:length(noms_groupes))))

    ind_sup <- ind_sup %>%
      right_join(inertie_sup, by = "ID") %>%
      right_join(coord_part_sup, by = "ID")
  }

  ind_sup <- ind_sup %>% mutate(type = "Supplementaire")

  ind_actifs %>%
    bind_rows(ind_sup) %>% # fusion des deux grands tableaux
    mutate(across(where(is.numeric), ~ round(.x, nb))) %>%
    relocate(ID, starts_with("Dim"), type)
}
