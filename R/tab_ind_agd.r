#' @title tab_ind_agd
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau contenant toutes les données des individus utilisés dans une analyse géométrique des données.
#'
#' @param agd objet AGD (MCA, MFA, PCA...)
#' @param nb nombre de chiffres à conserver pour toutes les statistiques produites.
#' @param id vecteur contenant les identifiants des individus dans la base de données originale (facultatif). Doit être dans le même ordre que les individus de la base de données de l'AGD.
#'
#' @return Un data.frame comprenant les coordonnées, contribbutions, cos2 (et inertie intra individuelle pour les AFM) des individus et les modalités de ces individus
#' @export
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr %>% select mutate bind_rows rename_all right_join relocate across
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_extract

tab_ind_agd <- function(agd, nb = Inf, id = NULL) {# creation de tous les tableaux pour les acm
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
      mutate(groupe = str_split_fixed(ID, "\\.", 2)[,2],
             ID = str_split_fixed(ID, "\\.", 2)[,1])

    ind_actifs <- ind_actifs %>%
      right_join(inertie, by = "ID") %>%
      right_join(coord_part, by = "ID")
  }

  ind_actifs <- ind_actifs %>% mutate(type = "Actif")

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
    coord_part_sup <- as.data.frame(agd$ind.sup$coord.partiel) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord_part")) %>%
      rownames_to_column("ID") %>%
      mutate(groupe = str_split_fixed(ID, "\\.", 2)[,2],
             ID = str_split_fixed(ID, "\\.", 2)[,1])

    ind_sup <- ind_sup %>%
      right_join(coord_part_sup, by = "ID")
  }

  ind_sup <- ind_sup %>% mutate(type = "Supplementaire")

  ind_tot <- ind_actifs %>%
    bind_rows(ind_sup) %>% # fusion des deux grands tableaux
    relocate(type, ID, starts_with("Dim"))

  if (type == "MFA") {
    ind_tot <- ind_tot %>% relocate(groupe, .after = type)
  }

  if (!is.null(id)) {
    if (!is.vector(id)) {
      stop("id doit etre un vecteur.")
    }
    if (length(id) %% nrow(ind_tot) != 0) {
      nb_id <- length(unique(ind_tot$ID))
      if (length(id) > nb_id) {
        stop(paste("id comporte", length(id) - nb_id, "elements en trop."))
      }
      if (length(id) < nb_id) {
        stop(paste("id comporte", nb_id - length(id), "elements en moins."))
      }
    }
    ind_tot <- ind_tot %>% mutate(ID = id[as.numeric(ID)])
  }

  ind_tot
}
