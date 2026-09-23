#' @title  desc_quanti
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir les principaux indicateurs concernant un ensemble de variables quantitatives
#'
#' @param data base de données
#' @param ... variables souhaitées (tidyselect)
#' @param moy TRUE par défaut. Moyenne de l'échantillon. Si FALSE, alors pas non plus d'écart-type.
#' @param sd TRUE par défaut. Ecart-type de l'échantillon
#' @param test.norm FALSE par défaut. P-value du test de normalité de l'échantillon (test de Shapiro-Wilk). Si la p-value est en dessous de votre seuil de significativité alors, l'hypothèse de normalité n'est pas retenue.
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param nb 1 par défaut. Nombre de décimales.
#' @param med TRUE par défaut. Médiane de l'échantillon
#' @param quant 4 par défaut. Nombre de quantiles. Si la médiane est sélectionnée, elle sera ajoutée si besoin. Si elle est exclue, on l'enlève des quantiles.
#' @param minmax TRUE par défaut. Minimum et maximum.
#' @param eff TRUE par défaut. Effectifs avec réponse.
#' @param eff_na FALSE par défaut. Ajoute une colonne avec le nombre de NA. N'apparait pas si eff = FALSE
#' @param msg FALSE par défaut. Envoie un message pour chaque variable terminée : utile si bug inexpliqué.
#' @param pond NULL par défaut. Vecteur contenant les poids (pas de tidyselect).
#' @param norm_pond FALSE par défaut. Normalise les poids (poids moyen = 1).
#'
#' @return un data.frame avec les indicateurs sélectionnés pour les variables quantitatives choisies
#'
#' @importFrom dplyr bind_rows mutate %>% filter group_by count rename select pull across summarise
#' @importFrom tidyselect everything
#' @importFrom stringr str_remove_all str_replace
#' @importFrom purrr map map_dfr map_lgl keep every
#' @importFrom Hmisc wtd.quantile
#' @importFrom rlang exec
#'
#' @export

desc_quanti <- function(data, ..., moy = TRUE, sd = TRUE, test.norm = FALSE, ic = TRUE, ic_seuil = 0.05, nb = 1, med = TRUE,
                        quant = 4, minmax = TRUE, eff = TRUE, eff_na = FALSE, msg = FALSE, pond = NULL, norm_pond = FALSE) {

  if (!moy) {
    ic <- FALSE
    sd <- FALSE
  }
  # tidyselect des variables a utiliser :
  data_vars <- data %>% select(...)
  if (ncol(data_vars) == 0) {
    warning("Pas de variable selectionnee, utilisation de toutes les variables numeriques.", call. = FALSE)
    data_vars <- data %>% select(everything() & where(is.numeric))
  }
  if (!(data_vars %>% every(is.numeric))) {
    list_num <- data_vars %>% select(!where(is.numeric)) %>% names()
    data_vars <- data_vars %>% keep(is.numeric)
    warning(paste0("Variable(s) ignoree(s) car non-numerique(s) : ", paste(list_num, collapse = ", ")), call. = FALSE)
  }
  # liste des noms :
  list_vars <- map(rlang::set_names(names(data_vars)), ~ rlang::quo(!!as.name(.x)))

  # gestion de la ponderation (si vecteur ou non + pb de longueur)
  if (!is.null(pond)) {# si pas de ponderation alors pas besoin de la normaliser
    if (nrow(data_vars) != length(pond) | any(is.na(pond))) {
      stop("Le vecteur de ponderation n'est pas de la bonne longueur ou contient des NA.")
    }
    if (norm_pond) {
      pond <- pond / mean(pond)
    }
  }

  # fonction de tri a plat par variable
  desc <- function(var) {
    # on selectionne la variable
    tab <- data %>% select({{var}})
    nom <- names(tab)

    # on cree une fonction donnant tous les indicateurs selectionnes
    # listes ou on ajoute au fur et a mesure :
    fonc <- list()

    if (eff) {# effectifs ou non
      fonc[[length(fonc) + 1]] <- list(N = function(x) {sum(!is.na(x))})
    } else eff_na <- FALSE

    if (eff_na) {# nombre de NA de la variable quanti ou non
      fonc[[length(fonc) + 1]] <- list(NR = function(x) {sum(is.na(x))})
    }

    if (moy | ic | sd) {
      # pour les indicateurs classiques on realise une regression qui est utile pour prendre en compte la ponderation
      fonc[[length(fonc) + 1]] <- list(Stats = function(x) {
        model <- lm(x ~ 1, data = tab, weights = pond)

        # calcul de la moyenne
        if (moy) {
          Moy <- data.frame(Moy = stats::coef(model))
        } else Moy <- NULL

        # ecart-type ?
        if (sd) {
          SD <- data.frame(SD = stats::sigma(model))
        } else SD <- NULL

        # intervalle de confiance ?
        if (ic) {
          IC <- stats::confint(model, level = (1 - ic_seuil)) %>%
            as.data.frame() %>%
            rename(`IC-` = `2.5 %`, `IC+` = `97.5 %`)
        } else IC <- NULL
        return(bind_cols(Moy, IC, SD))
      })
    }

    if (quant > 0 | med | minmax) {
      if (quant == 0 & med) {
        # dans ce cas on veut deux quantiles
        quant <- 2
      }
      if (quant == 0 & !med) {
        # dans ce cas on veut que les min et max
        quant <- 1
      }

      # transformation du nombre de quantiles en bornes
      val_quant <- seq(0, 1, 1 / quant)

      if (med & quant %% 2) {
        # si on veut la mediane et qu'elle est pas contenue dans les quantiles alors on l'ajoute au milieu des bornes
        val_quant <- c(val_quant, 0.5)
        val_quant <- val_quant[order(val_quant)]
      }

      if (!med & any(val_quant == .5)) {
        # on enleve la mediane si elle est explicitement retiree des arguments
        val_quant <- val_quant[val_quant != .5]
        message("Mediane retiree des quantiles malgre sa comptabilisation car med = FALSE")
      }

      if (!minmax) {
        # on enleve les bornes 0 et 1 qui sont les min et max
        val_quant <- val_quant[!val_quant %in% c(0, 1)]
      }

      fonc[[length(fonc) + 1]] <- list(Quantiles = function(x) {
        Hmisc::wtd.quantile(x, weights = pond, probs = val_quant, na.rm = TRUE, normwt = TRUE) %>%
          t() %>%
          as.data.frame() %>%
          rename_with(~.x %>%
                        str_remove_all("\\s") %>%
                        str_replace("^0%", "Min") %>%
                        str_replace("^50%", "Med") %>%
                        str_replace("100%", "Max"))
      }
      )
    }

    if (test.norm) {
      fonc[[length(fonc) + 1]] <- list(`Shapiro-Wilk` = function(x) {
        shap <- stats::shapiro.test(x)$p.value
        return(format(shap, digits = 2, scientific = ifelse(shap < 0.001, T, F)))
      }
      )
    }
    # on a une liste de liste alors qu'on veut qu'une liste
    fonc <- unlist(fonc)

    suppressWarnings(
      # on applique toutes les fonctions selectionnees a la variable quanti
      tab <- tab %>% summarise(map_dfc(fonc, ~exec(.x, {{var}})))
    )
    if (msg) {
      message(paste(nom, "------------------ OK"))
    }
    return(tab)
  }

  # boucle pour toute la liste de variables :
  map_dfr(list_vars, ~desc(!!.x), .id = "Variable") %>%
    mutate(across(where(is.numeric), ~round(.x, nb))) %>%
    return()
}


#' @title  desc_quali
#' @author Kanto Fiaferana & Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tri à plat de plusieurs variables catégorielles en même temps
#'
#' @param data base de données
#' @param ... variables souhaitées
#' @param eff TRUE par défaut. Fait apparaître les effectifs par catégorie.
#' @param freq TRUE par défaut. idem mais avec les fréquences.
#' @param nb 1 par défaut. Nombre de décimales pour les pourcentages.
#' @param cum_freq FALSE par défaut. Fréquences cumulées (veillez bien à ranger les modalités dans l'ordre souhaité)
#' @param NR FALSE par défaut. Les non-réponses comptent-elles parmi les catégories ?
#' @param tot TRUE par défaut. Ligne de total à la fin de chaque variable.
#' @param pond NULL par défaut. Vecteur contenant les poids.
#' @param norm_pond FALSE par défaut. Normalise les poids (poids moyen = 1).
#'
#' @return un tableau avec les effectifs et fréquences de chaque modalité de chaque variable sélectionnée
#'
#' @importFrom dplyr bind_rows mutate %>% filter group_by count rename select pull across
#' @importFrom tidyselect everything
#' @importFrom purrr map map_dfr
#'
#' @export

desc_quali <- function(data, ..., eff = TRUE, freq = TRUE, nb = 1, cum_freq = FALSE, NR = FALSE, tot = TRUE, pond = NULL,
                       norm_pond = FALSE) {
  if (cum_freq) {# si freq cumule alors forcement non cumule
    freq <- TRUE
  }
  # tidyselect des variables a utiliser :
  data_vars <- data %>% select(...)
  if (ncol(data_vars) == 0) {
    warning("Pas de variable selectionnee, utilisation de toutes les variables categorielles.", call. = FALSE)
    data_vars <- data %>% select(everything() & !where(is.numeric))
  }
  # liste des noms :
  list_vars <- map(rlang::set_names(names(data_vars)), ~ rlang::quo(!!as.name(.x)))

  # gestion de la ponderation (si vecteur ou non + pb de longueur)
  if (!is.null(pond)) {# si pas de ponderation alors pas besoin de la normaliser
    if (nrow(data_vars) != length(pond)) {
      stop("Le vecteur de ponderation n'est pas de la bonne longueur.")
    }
  } else norm_pond <- FALSE

  # fonction de tri a plat par variable
  desc <- function(var) {
    # on selectionne la variable
    tab <- data %>% select({{var}})
    test <- pull(tab)
    nom <- names(tab)
    if (is.numeric(test)) {
      warning(paste("La variable" , nom, "n'est pas categorielle mais numerique !\nTransformee en categorielle."), call. = FALSE)
    }
    # si ponderation on ajoute au nouveau dataframe :
    if (!is.null(pond)) {
      # on ajoute le vecteur de poids
      tab <- tab %>% mutate(pond = pond)
    }

    if (!NR) {
      tab1 <- tab
      # on enleve les NA
      tab <- tab %>% filter(!is.na({{var}}))
      # s'il n'y a que des NA alors pas de lignes
      if (nrow(tab) == 0) {
        warning(paste(names(tab)[1]), " ne contient que des non-reponses, elles sont gardees pour cette variable.", call. = FALSE)
        tab <- tab1
      }
      # normalisation de la ponderation si besoin et si demande
      if (norm_pond & sum(pond, na.rm = T) != nrow(tab)) {
        tab <- tab %>% mutate(pond = pond / mean(pond, na.rm = T))
      }
    }
    # si pas de ponderation on fait juste les effectifs
    if (is.null(pond)) {
      tab <- tab %>% count({{var}}, name = "N")
    } else { # sinon, somme des poids
      tab <- tab %>% count({{var}}, wt = pond, name = "N")

    }
    if (freq) {# ajout des frequences
      tab <- tab %>% mutate(Frequence = 100 * N / sum(N))
    }

    if (cum_freq) {# ajout des frequences cumulees
      tab <- tab %>% mutate(`Frequence cumulee` = cumsum(Frequence))
    }
    if (!eff) {# si pas les effectifs, on les enleve
      tab <- tab %>% select(-N)
    }
    # on renomme la colonne 1 "modalites" et on la met en character
    tab <- tab %>%
      rename(Modalite = 1) %>%
      mutate(Modalite = as.character(Modalite))
    # ajout d'une ligne total
    if (tot) {
      tab <- tab %>%
        bind_rows(summarise(., across(where(is.numeric), sum), across(!where(is.numeric), ~'Total')))
      if (cum_freq) {
        tab <- tab %>%
          mutate(`Frequence cumulee` = case_when(Modalite == 'Total' ~ NA, TRUE ~ `Frequence cumulee`))
      }
    }
    tab %>% mutate(across(where(is.numeric), ~round(.x, nb)))
  }

  # boucle pour toute la liste :
  map_dfr(list_vars, ~desc(!!.x), .id = "Variable")
}
