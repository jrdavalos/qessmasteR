#' @title multi_quanti
#' @author  Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau détaillant une variable quantitative en fonction d'une liste de variables catégorielles.
#'
#' @param data base de données. Pas nécessaire s'il y a un survey-design réalisé.
#' @param var_princ variable principale (quantitative, en colonnes)
#' @param ... variables catégorielles à croiser avec la variable principale (en lignes)
#' @param moy TRUE par défaut. Moyenne par modalité.
#' @param test.diffmoy `TRUE` par défaut. Réalise un test global d'égalité des moyennes entre les modalités, tenant compte du design avec `survey::svyglm()` et `survey::regTermTest()`.
#' Ce test repose sur une approximation asymptotique et doit être interprété avec prudence lorsque les effectifs ou le nombre de grappes sont faibles, ou lorsque la distribution comporte des valeurs extrêmes.
#' @param sd TRUE par défaut. Écart-type par modalité.
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne par modalité. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance des moyennes.
#' @param nb nombre de décimales pour la moyenne, l'écart-type et l'intervalle de confiance.
#' @param med TRUE par défaut. Médiane par modalité.
#' @param quant 4 par défaut. Nombre de quantiles. Si la médiane est sélectionnée, elle sera ajoutée si besoin. Prend 1 ou moins si pas de quantiles souhaités.
#' @param minmax TRUE par défaut. Minimum et maximum par modalité.
#' @param eff `TRUE` par défaut. Effectif pondéré des réponses valides à la variable quantitative par modalité.
#' @param freq `TRUE` par défaut. Fréquence pondérée des réponses valides à la variable quantitative par modalité.
#' @param eff_na `FALSE` par défaut. Effectif pondéré des non-réponses à la variable quantitative par modalité.
#' @param signif NULL par défaut, prend les valeurs 'etoiles' ou 'seuils'. Permet d'afficher les résultats des tests avec des étoiles ou avec des seuils à la place de la p.value en clair.
#' @param design survey-design si déjà créé.
#' @param pond variable de pondération, le cas échéant
#' @param norm_pond `FALSE` par défaut. Normalise les poids utilisés pour les effectifs affichés afin que leur somme corresponde au nombre d'observations conservées.
#' Cette normalisation n'affecte pas le design utilisé pour les moyennes, intervalles de confiance, quantiles et tests.
#' @param NR FALSE par défaut. Garde les non-réponses des variables catégorielles.
#' @param msg FALSE par défaut. Envoie un message pour chaque variable terminée : utile si bug inexpliqué.
#'
#' @return Un tibble avec en colonne les indicateurs synthétiques de la variable d'intérêt selon les modalités des variables catégorielles choisies.
#' @export
#'
#' @importFrom purrr map map_dfc list_rbind
#' @importFrom rlang quo exec set_names
#' @importFrom dplyr group_by summarise select rename pull cur_group_rows mutate case_when everything where

multi_quanti = function(data = NULL, var_princ, ..., moy = TRUE, test.diffmoy = TRUE, sd = TRUE, ic = TRUE,
                         ic_seuil = 0.05, nb = 2, med = TRUE, quant = 4, minmax = TRUE, eff = TRUE, eff_na = FALSE, freq = TRUE,
                         signif = NULL, pond = NULL, norm_pond = FALSE, design = NULL, msg = FALSE, NR = FALSE) {
  if (!moy) {
    ic = FALSE
    sd = FALSE
  }
  if (quant <= 1) {
    quant = 0
    warning("Pas de quantile calcule car quant <= 1.")
  }
  if (!is.null(signif) && !(signif %in% c("etoiles", "seuils"))
  ) {
    warning("`signif` doit prendre la valeur 'etoiles' ou 'seuils'. La p-value est affichee en clair.",
            call. = FALSE)
    signif = NULL
  }

  # design pour la ponderation
  design = creer_design(data = data, pond = {{pond}}, design = design)
  data <- design$variables

  nom_princ = data |> select({{var_princ}}) |> names()
  formule_princ = stats::reformulate(nom_princ)

  sommaire = function(var) {
    # on teste si la variable est bien categorielle
    test = data |> select({{var}}) |> pull()
    nom = data |> select({{var}}) |> names()
    if (is.numeric(test)) {
      warning(paste("La variable" , nom, "n'est pas categorielle mais numerique !\nTransformee en categorielle."), call. = FALSE)
    }

    # on cree le design de la variable etudiee
    design_var = design

    if (!NR) {
      lignes = !is.na(design_var$variables[[nom]])

      if (!any(lignes)) {
        warning(paste(nom, "ne contient que des non-reponses, elles sont gardees pour cette variable."),
                call. = FALSE)
        } else {
          design_var = design_var[lignes, ]
        }
      }

    tab = design_var$variables

    poids_var = stats::weights(design_var, type = "sampling")

    if (norm_pond) {
      somme_poids = sum(poids_var, na.rm = TRUE)

      if (somme_poids > 0) {
        poids_var = poids_var * nrow(design_var$variables) / somme_poids
      }
    }

    # fonctions servant a trier les poids et le design a chaque modalite
    trieur_poids = function() {
      poids_var[cur_group_rows()]
    }

    trieur_design = function() {
      design_var[cur_group_rows(),]
    }

    # on cree une fonction donnant tous les indicateurs selectionnes
    # listes ou on ajoute au fur et a mesure les operations:
    fonc = list()

    if (eff) {
      fonc[[length(fonc) + 1]] <- list(
        N = function(x) {
          poids_groupe = trieur_poids()
          sum(poids_groupe[!is.na(x)], na.rm = TRUE)
        }
      )
    }
    if (freq) {
      fonc[[length(fonc) + 1]] = list(
        Freq = function(x) {
          poids_groupe = trieur_poids()
          round(100 * sum(poids_groupe[!is.na(x)], na.rm = TRUE) / sum(poids_var, na.rm = TRUE), nb)
          }
        )
    }
    if (eff_na) {# nombre de NA de la variable quanti ou non
      fonc[[length(fonc) + 1]] = list(
        NR = function(x) {
          poids_groupe = trieur_poids()
          sum(poids_groupe[is.na(x)], na.rm = TRUE)
          }
        )
    }
    if (minmax) {
      fonc[[length(fonc) + 1]] <- list(Min = function(x) {if (all(is.na(x))) {NA_real_} else {min(x, na.rm = TRUE)}})
      fonc[[length(fonc) + 1]] <- list(Max = function(x) {if (all(is.na(x))) {NA_real_} else {max(x, na.rm = TRUE)}})
    }
    if (ic) {
      fonc[[length(fonc) + 1]] <- list(
        Moyenne = function(x) {
          estimation <- survey::svymean(formule_princ,
                                        design = trieur_design(),
                                        na.rm = TRUE)

          bornes <- stats::confint(estimation, level = 1 - ic_seuil)

          data.frame(Moy = round(unname(stats::coef(estimation)[1]), nb),
                     `IC-` = round(bornes[1, 1], nb),
                     `IC+` = round(bornes[1, 2], nb),
                     check.names = FALSE)
          }
        )
      } else if (moy) {
        fonc[[length(fonc) + 1]] <- list(
          Moy = function(x) {
            estimation <- survey::svymean(formule_princ,
                                          design = trieur_design(),
                                          na.rm = TRUE)

          round(unname(stats::coef(estimation)[1]), nb)
        }
      )
    }
    if (sd) {
      fonc[[length(fonc) + 1]] <- list(
        SD = function(x) {
          variance <- survey::svyvar(formule_princ,
                                     design = trieur_design(),
                                     na.rm = TRUE)

          round(sqrt(unname(stats::coef(variance)[1])), nb)
        }
      )
    }
    if (quant > 0) {
      # Bornes des quantiles
      val_quant <- seq(0, 1, 1 / quant)

      # Ajout de la mediane si elle n'est pas deja presente
      if (med && quant %% 2) {
        val_quant <- c(val_quant, 0.5)
        val_quant <- val_quant[order(val_quant)]
      }

      # Suppression de la mediane si elle est explicitement retiree
      if (!med) {
        val_quant <- val_quant[val_quant != 0.5]
      }

      # Suppression du minimum et du maximum
      val_quant <- val_quant[val_quant > 0 & val_quant < 1]

      fonc[[length(fonc) + 1]] <- list(
        Quantiles = function(x) {
          estimation <- survey::svyquantile(formule_princ,
                                            design = trieur_design(),
                                            quantiles = val_quant,
                                            na.rm = TRUE,
                                            ci = FALSE)

          valeurs <- as.numeric(stats::coef(estimation))

          noms <- ifelse(val_quant == 0.5, "Med", paste0(format(100 * val_quant, trim = TRUE, scientific = FALSE), "%"))

          valeurs |>
            round(nb) |>
            rlang::set_names(noms) |>
            as.list() |>
            as.data.frame(check.names = FALSE)
        }
      )

    } else if (med) {
      fonc[[length(fonc) + 1]] <- list(
        Mediane = function(x) {
          estimation <- survey::svyquantile(formule_princ,
                                            design = trieur_design(),
                                            quantiles = 0.5,
                                            na.rm = TRUE,
                                            ci = FALSE)

          round(as.numeric(stats::coef(estimation))[1], nb)
        }
      )
    }


    # on a une liste de listes alors qu'on veut qu'une liste
    fonc <- unlist(fonc, recursive = FALSE)

    # on cree le tableau
    tab = suppressWarnings(
      tab |>
        # par categorie
        group_by({{var}}) |>
        # on applique toutes les fonctions selectionnees a la variable quanti
        summarise(map_dfc(fonc, ~exec(.x, {{var_princ}})))
    )

    if (test.diffmoy) {
      # Design de la variable en cours
      design_test <- design_var

      # Suppression des valeurs manquantes sur les deux variables et trasnformation en facteur
      design_test <- design_test[!is.na(design_test$variables[[nom_princ]]) & !is.na(design_test$variables[[nom]]), ]

      modalites_test <- unique(design_test$variables[[nom]])

      if (length(modalites_test) < 2) {
        pval.test <- NA_real_
        warning(paste("Le test d'egalite des moyennes n'a pas ete realise pour", nom, "car la variable comporte moins de deux modalites valides."),
                call. = FALSE)
      } else {
        design_test$variables[[nom]] <- as.factor(design_test$variables[[nom]])

        # Formule : variable quantitative ~ variable categorielle
        formule_test <- stats::reformulate(termlabels = nom, response = nom_princ)

        # Regression lineaire tenant compte du plan de sondage
        modele <- survey::svyglm(formule_test, design = design_test, family = stats::gaussian())

        # Test global de la variable categorielle
        resultat_test <- survey::regTermTest(modele, stats::reformulate(nom), method = "Wald")

        pval.test <- resultat_test$p
      }

      if (!is.null(signif)) {
        if (signif == "etoiles") {
          tab = tab |>
            mutate(P.val = case_when(is.na(pval.test) ~ NA_character_,
                                     pval.test < 0.01 ~ "***",
                                     pval.test < 0.05 ~ "**",
                                     pval.test < 0.1 ~ "*",
                                     TRUE ~ ""))
          } else if (signif == "seuils") {
          tab <-tab |>
            mutate(P.val = case_when(is.na(pval.test) ~ NA_character_,
                                     pval.test < 0.001 ~ "< 0.001",
                                     pval.test < 0.01 ~ "< 0.01",
                                     pval.test < 0.05 ~ "< 0.05",
                                     pval.test < 0.1 ~ "< 0.1",
                                     TRUE ~ "ns"))
          }
        } else {
          tab = tab |> mutate(Test = ifelse(is.na(pval.test), NA_character_,
                                            paste("P(Wald) =", format(pval.test,digits = 2, scientific = pval.test < 0.001)))
                              )
        }
      }

    if (moy) {
      if (sum(is.na(tab$Moy)) > 0)
      warning(paste("Pas de moyenne calculee : la variable" , nom,
                    "comporte au moins une modalite avec uniquement des non reponses au croisement avec la variable d'interet."),
              call. = FALSE)
    }

    if (sd & ic) {
      if (sum(is.na(tab$SD)) > 0)
      warning(paste("Pas d'ecart-type ou d'intervalle de confiance calcules : la variable" , nom, "comporte au moins une modalite avec une unique reponse au croisement de la variable d'interet."), call. = FALSE)
    } else if (sd) {
      if (sum(is.na(tab$SD)) > 0)
      warning(paste("Pas d'ecart-type calcules : la variable" , nom, "comporte au moins une modalite avec une unique reponse au croisement de la variable d'interet."), call. = FALSE)
    } else if (ic) {
      if (sum(is.na(tab$`IC-`)) > 0)
      warning(paste("Pas d'intervalle de confiance calcules : la variable" , nom, "comporte au moins une modalite avec une unique reponse au croisement de la variable d'interet."), call. = FALSE)
    }
    if (msg) {
      message(paste(nom, "------------------ OK"))
    }

    tab |>
      rename(Modalite = 1) |>
      mutate(Modalite = as.character(Modalite))

  }

  # d'abord le dataframe avec uniquement les variables souhaitees :
  data_vars = data |> select(...)
  if (ncol(data_vars) == 0) {
    warning("Pas de variable selectionnee a croiser avec ", deparse(substitute(var_princ)),
            ".\nUtilisation de toutes les autres variables categorielles.", call. = FALSE)
    data_vars = data |> select(everything() & !where(is.numeric) & -{{var_princ}})
  }
  # on peut faire la liste des NOMS comme suit :
  list_vars = map(rlang::set_names(names(data_vars)), ~ rlang::quo(!!as.name(.x)))

  # on applique nos fonctions a la list :
  map(list_vars, ~sommaire(!!.x)) |>
    list_rbind(names_to = "Variable")
}

#' @title multi_croise
#' @author Julio Ricardo Davalos
#'
#'
#' @description Permet d'obtenir un tableau croisant une variable d'intérêt avec une liste d'autres variables.
#'
#' @param data Base de données. Peut être omise si un `design` valide est fourni.
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
#' @param pond Variable de pondération facultative, à fournir sans guillemets. Utilisée si aucun `design` valide n'est fourni.
#' @param design Objet créé avec `survey::svydesign()` ou `survey::svrepdesign()`.
#' @param norm_pond `FALSE` par défaut. Normalise les effectifs pondéré saffichés afin que leur somme corresponde au nombre d'observations du tableau.
#'  Cette normalisation ne modifie pas le plan utilisé pour le test d'indépendance.
#'
#' @return Un tabyl data.frame regroupant tous les tableaux croisés avec pourcentages et effectifs. Si les pourcentages sont en ligne et que les totaux sont activés alors la ligne de total est nommée "Ensemble" et la colonne de total est nommée "Total" et inversement pour les pourcentages en colonne.
#' @export
#'
#' @importFrom purrr map_dfr map
#' @importFrom dplyr mutate rename select case_when everything where
#' @importFrom janitor adorn_totals adorn_percentages adorn_pct_formatting adorn_ns adorn_rounding as_tabyl

multi_croise = function(data = NULL, var_princ, ..., NR = FALSE, pct_ligne = TRUE, nb = 1, p.val = FALSE,
                        cram.v = FALSE, sign = 0.05, tot = c("row", "col"), eff = TRUE, pourcent = FALSE,
                        pond = NULL, design = NULL, norm_pond = FALSE) {

  # Le V de Cramer implique l'affichage de la p-value
  if (cram.v) {
    p.val = TRUE
  }

  # Creation ou validation du plan
  design = creer_design(data = data, pond = {{pond}}, design = design)

  # Les donnees contenues dans le plan deviennent la reference
  data = design$variables

  nom_princ = data |> select({{var_princ}}) |> names()

  tableau = function(var) {
    nom = data |> select({{var}}) |> names()
    design_var = design

    # Conservation eventuelle des non-reponses
    if (NR) {
      variable_ligne = as.character(design_var$variables[[nom]])
      variable_ligne[is.na(variable_ligne)] = "NR"
      design_var$variables[[nom]] = factor(variable_ligne)

      variable_colonne = as.character(design_var$variables[[nom_princ]])
      variable_colonne[is.na(variable_colonne)] = "NR"
      design_var$variables[[nom_princ]] = factor(variable_colonne)

    } else {
      # Suppression des observations manquantes sur les deux variables
      lignes = !is.na(design_var$variables[[nom]]) & !is.na(design_var$variables[[nom_princ]])

      if (!any(lignes)) {
        stop("Aucune observation complete pour le croisement entre `",
             nom, "` et `", nom_princ, "`.",
             call. = FALSE)
      }

      design_var = design_var[lignes, ]
    }

    # Conversion explicite en facteurs
    design_var$variables[[nom]] = factor(design_var$variables[[nom]])
    design_var$variables[[nom_princ]] = factor(design_var$variables[[nom_princ]])

    formule_tableau = stats::reformulate(c(nom, nom_princ), response = NULL)

    # Tableau d'effectifs ponderes
    tab_matrice = survey::svytable(formule_tableau, design = design_var)

    # Normalisation facultative des effectifs affiches
    if (norm_pond) {
      total_pondere = sum(tab_matrice, na.rm = TRUE)
      if (total_pondere > 0) {
        tab_matrice = tab_matrice * nrow(design_var$variables) / total_pondere
      }
    }

    # Passage au format tabyl attendu par la suite
    tab = tab_matrice |>
      as.data.frame.matrix() |>
      tibble::rownames_to_column(var = nom) |>
      as_tabyl()

    list(tab = tab, design = design_var, formule = formule_tableau, matrice = tab_matrice, nom = nom)
  }

  transformation = function(resultat) {
    tabl = resultat$tab
    design_var = resultat$design
    formule_tableau = resultat$formule
    tab_matrice = resultat$matrice
    nom = resultat$nom

    cram.v_var = cram.v

    # Test d'independance tenant compte du plan
    if (p.val) {
      test_chi2 = tryCatch(survey::svychisq(formule_tableau, design = design_var, statistic = "F"),
                           error = function(e) NULL)

      if (is.null(test_chi2)) {
        p = NA_real_
        cram.v_var = FALSE

        warning(paste0("Pas de p-value calculable pour la variable : ", nom),
                call. = FALSE)

      } else {
        p = test_chi2$p.value

        if (is.na(p)) {
          cram.v_var = FALSE
          warning(paste0("Pas de p-value calculable pour la variable : ", nom),
                  call. = FALSE)

        } else if (p > sign) {
          cram.v_var = FALSE
        }
      }

      p_affichee = if (is.na(p)) {""} else {format(p, digits = 2, scientific = p < 0.001)}
    }

    # V de Cramer descriptif sur le tableau pondere
    if (cram.v_var) {
      test_cramer = tryCatch(suppressWarnings(stats::chisq.test(tab_matrice, correct = FALSE)),
                             error = function(e) NULL)

      if (is.null(test_cramer)) {

        cram.v_var = FALSE

      } else {
        n = sum(tab_matrice, na.rm = TRUE)

        dimension = min(nrow(tab_matrice), ncol(tab_matrice)) - 1

        if (n > 0 && dimension > 0 && !is.na(test_cramer$statistic)) {

          v = sqrt(as.numeric(test_cramer$statistic) / (n * dimension))
          v = paste("V =", format(v, digits = 2))

        } else {

          cram.v_var = FALSE
        }
      }
    }

    tab_tot = tabl |>
      adorn_totals(where = tot, name = case_when(pct_ligne ~ c("Ensemble", "Total"),
                                                 TRUE ~ c("Total", "Ensemble"))) |>
      adorn_rounding(0)

    tab = tab_tot |>
      adorn_percentages(denominator = ifelse(pct_ligne, "row", "col")) |>
      adorn_pct_formatting(digits = nb, rounding = "half up", affix_sign = pourcent)

    if (eff) {
      tab = tab |> adorn_ns(ns = tab_tot)
    }

    if (p.val) {
      if (cram.v_var) {
        tab = tab |>
          mutate(Test = c(rep(p_affichee, nrow(tab) - 1), v))

      } else {
        tab = tab |>
          mutate(Test = rep(p_affichee, nrow(tab)))
      }
    }

    tab |> rename(Modalite = 1)
  }

  # Variables a croiser avec la variable principale
  data_vars = data |> select(...)

  if (ncol(data_vars) == 0) {

    warning("Pas de variable selectionnee a croiser avec ",
            deparse(substitute(var_princ)),
            ".\nUtilisation de toutes les autres variables categorielles.",
            call. = FALSE)

    data_vars = data |>
      select(everything() & !where(is.numeric) & -{{ var_princ }})
  }

  list_vars = map(rlang::set_names(names(data_vars)), ~ rlang::quo(!!as.name(.x)))

  map_dfr(list_vars, ~ transformation(tableau(!!.x)), .id = "Variable")
}


#' @title creer_design
#' @author  Julio Ricardo Davalos
#'
#' @description Créer ou contrôler un plan de sondage
#'
#' Fonction interne utilisée pour construire un plan de sondage simple à partir d'une variable de pondération, ou pour contrôler un plan fourni par l'utilisateur.
#'
#' @param data Base de données.
#' @param pond Variable de pondération. Utilise la sélection non standard.
#' @param design Objet `survey.design` ou `svyrep.design`.
#'
#' @return Un objet de plan de sondage.
#'
#' @keywords internal
#' @noRd
#'
creer_design = function(data = NULL, pond = NULL, design = NULL) {
  pond_quo = rlang::enquo(pond)

  if (!is.null(design)) {

    # Verification de la classe du plan
    if (!inherits(design, c("survey.design", "survey.design2", "svyrep.design"))) {
      # s'il y a pond alors on peut le garder
      if (rlang::quo_is_null(pond_quo)) {
        stop("`design` doit etre un objet cree avec `survey::svydesign()` ou `survey::svrepdesign()`.",
             call. = FALSE)
      } else {
        message("design n'est pas cree avec `survey::svydesign()` ou `survey::svrepdesign()`. On utilise pond.")
        design = NULL
      }
    }
    if (!is.null(design)) {
      # Si data n'est pas fournie, le design suffit
      if (is.null(data)) {
        return(design)
      }
      data_design = design$variables

      # verif du nb de lignes
      if (nrow(data) != nrow(data_design)) {
        # s'il y a pond alors on peut le garder
        if (rlang::quo_is_null(pond_quo)) {
          stop("`data` et `design` ne contiennent pas le meme nombre d'observations.",
               call. = FALSE)
          } else {
          message("`data` et `design` ne contiennent pas le meme nombre d'observations. On utilise pond.")
          design = NULL
          }
        }
      # verif des donnees design
      if (!is.null(design) && !isTRUE(all.equal(data, data_design, check.attributes = FALSE))) {
        message("`data` est different de `design$variables`.")
        design$variables = data
        }
      }
    } # si le design est ok, il sera utilise tel quel

  if (is.null(data) && is.null(design)) {
    stop("`data` doit etre fourni en l'absence d'un `design` valide.",
         call. = FALSE)
  }

  if (!rlang::quo_is_null(pond_quo) && is.null(design)) { # matche si design == NULL ou s'il est incompatible

    nom_pond = rlang::as_name(pond_quo)

    if (!nom_pond %in% names(data)) {
      stop("La variable de ponderation `", nom_pond, "` n'existe pas dans `data`.",
           call. = FALSE)
    }

    valeurs_pond = data[[nom_pond]]

    if (!is.numeric(valeurs_pond)) {
      stop("La variable de ponderation doit etre numerique.",
           call. = FALSE
      )
    }

    if (anyNA(valeurs_pond) | any(valeurs_pond < 0)) {
      stop("La variable de ponderation contient des valeurs manquantes ou negatives.",
           call. = FALSE
      )
    }

    formule_pond = stats::reformulate(nom_pond)

    design = survey::svydesign(ids = ~1, weights = formule_pond, data = data)

  } else if (is.null(design)) {# Aucun plan et aucun pond : plan non pondere
    design = survey::svydesign(ids = ~1, weights = ~1, data = data)
  }

  return(design)
}
