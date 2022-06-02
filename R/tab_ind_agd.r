#' @title tab_ind_agd
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau contenant toutes les données de toutes les variables utilisées dans une analyse géométrique des données.
#'
#' @param agd objet AGD (MCA, MFA, PCA...)
#' @param sup_dbl TRUE par défaut. La fonction colle le nom de la variable à la modalité en cas de doublon. Ici, on supprime cette concaténation pour plus de lisibilité dans le tableau final. Peut gêner la lecture dans le cas d'une représentation graphique basée uniquement sur les modalités.
#' @param nb nombre de chiffres à conserver pour toutes les statistiques produites.
#'
#' @return Un data.frame comprenant le nom des variables, modalités, si elles sont actives ou supplémentaires ainsi que l'ensemble des données générées par l'AGD (coordonnées, cos2, contributiosn etc.).
#'
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map map_dbl map2
#' @importFrom dplyr %>% rename select mutate left_join bind_cols bind_rows rename_all rename_with group_by count ungroup add_count filter case_when right_join relocate across last_col
#' @importFrom tidyselect starts_with ends_with matches everything
#' @importFrom stringr str_detect str_extract str_remove

tab_ind_agd <- function(agd, sup_dbl = TRUE, nb = 3) {# creation de tous les tableaux pour les acm
  type <- class(agd)[1]

  if (type == "MCA") {
    nom_dim <- "dim"
    donnees <- get(agd$call$call$X)
    quali.act <- agd$var
    quali.sup <- agd$quali.sup
  } else if (type == "AFC") {
    # a finaliser
  } else if (type == "MFA") {
    nom_dim <- "comp"
    donnees <- get(agd$call$call$base)
    quali.act <- agd$quali.var
    quali.sup <- agd$quali.var.sup

    groupes <- agd$summary.quali %>%
      rename(groupe = group) %>%
      select(variable, modalite, groupe) %>%
      mutate(groupe = agd$call$name.group[groupe]) %>%
      left_join(bind_cols(bind_rows(as.data.frame(agd$group$coord), as.data.frame(agd$group$coord.sup)) %>%
                            rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord")),
                          bind_rows(as.data.frame(agd$group$cos2), as.data.frame(agd$group$cos2.sup)) %>%
                            rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_cos2"))) %>%
                  rownames_to_column("groupe"), by = "groupe") %>%
      left_join(bind_cols(as.data.frame(agd$group$correlation) %>% rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_corr")),
                          as.data.frame(agd$group$contrib) %>% rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_contrib"))) %>%
                  rownames_to_column("groupe"), by = "groupe") %>%
      rename_with(~ paste0(.x, "_group"), starts_with("Dim"))

  } else {message("Pas objet AGD ou type non encore finalise !")}

    # variable actives
    frequences <- donnees %>% # frequences de chaque modalite
      pivot_longer(everything(), names_to = "variable", values_to = "modalite") %>% # prend les modalite s
      count(variable, modalite) %>% # compte
      group_by(variable) %>% # regroupe
      mutate(pourcentage = 100 * n / nrow(donnees)) %>% # tri a plat des modalites
      ungroup() %>%
      select(variable, modalite, n, pourcentage)

    var_reco <- frequences %>%
      add_count(modalite, name = "nn") %>%
      filter(nn > 1) %>%
      select(variable, modalite)

    if (nrow(var_reco) > 0) {
      frequences <-  frequences %>%
        mutate(modalite = case_when(variable %in% var_reco$variable & modalite %in% var_reco$modalite ~
                                      case_when(is.na(modalite) ~ paste0(variable, ".NA"),
                                                TRUE ~ paste(variable, modalite, sep = "_")),
                                     TRUE ~ modalite))
    }

    if (type == "MFA") {
      frequences <- groupes %>% left_join(frequences, by = c("variable", "modalite"))
    }

    coordonnees <- as.data.frame(quali.act$coord) %>%  # coordonnee de chaque modalite sur chaque axe
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord")) %>%
      rownames_to_column("modalite")

    contributions <- as.data.frame(quali.act$contrib) %>% # contribution de chaque modalite a chaque axe
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_contrib")) %>%
      rownames_to_column("modalite")

    cos2 <- as.data.frame(quali.act$cos2) %>% # cos2 de chaque modalite sur chaque axe
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_cos2")) %>%
      rownames_to_column("modalite")

    vtest <- as.data.frame(quali.act$v.test) %>%  # test de chaque modalite sur chaque axe
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_vtest")) %>%
      rownames_to_column("modalite")

    resultats_actives <- frequences %>%  # fusion variables actives
      right_join(coordonnees, by = "modalite") %>%
      right_join(contributions, by = "modalite") %>%
      right_join(cos2, by = "modalite") %>%
      right_join(vtest, by = "modalite")

    if (type == "MCA") {
      eta2 <- as.data.frame(quali.act$eta2) %>%
        rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_eta2_var")) %>%
        rownames_to_column("variable")

      resultats_actives <- resultats_actives %>% left_join(eta2, by = "variable")
    }

    resultats_actives <- resultats_actives %>% mutate(type = "Active")

    # variable sup
    coordonnees_sup <-  # coordonnee de chaque modalite sur chaque axe
      as.data.frame(quali.sup$coord) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_coord")) %>%
      rownames_to_column("modalite")

    cos2_sup <-  # cos2 de chaque modalite sur chaque axe
      as.data.frame(quali.sup$cos2) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_cos2")) %>%
      rownames_to_column("modalite")

    vtest_sup <-  # test de chaque modalite sur chaque axe
      as.data.frame(quali.sup$v.test) %>%
      rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_vtest")) %>%
      rownames_to_column("modalite")

    resultats_sup <- frequences %>% # fusion variables sup
      right_join(coordonnees_sup, by = "modalite") %>%
      right_join(cos2_sup, by = "modalite") %>%
      right_join(vtest_sup, by = "modalite")

    if (type == "MCA") {
      eta2_sup <- as.data.frame(quali.sup$eta2) %>%
        rename_all(~ paste0("Dim", str_extract(.x, "\\d+"), "_eta2_var")) %>%
        rownames_to_column("variable")

      resultats_sup <- resultats_sup %>% left_join(eta2_sup, by = "variable")
    }

    resultats_sup <- resultats_sup %>% mutate(type = "Supplementaire")

    resultats_tot <- bind_rows(resultats_actives, resultats_sup) %>% # fusion des deux grands tableaux
      mutate(across(where(is.numeric), ~ round(.x, nb))) %>%
      relocate(type, variable, modalite, n, pourcentage) %>%
      relocate(ends_with("_group"), .after = last_col()) %>%
      relocate(matches("groupe")) %>%
      filter(!is.na(variable))

    if (sup_dbl) {
      resultats_tot %>% mutate(modalite = case_when(str_detect(modalite, ".NA") ~
                                                      str_remove(modalite, paste0(variable, ".")),
                                                     TRUE ~ str_remove(modalite, paste0(variable, "_"))))
    }
    else resultats_tot
}
