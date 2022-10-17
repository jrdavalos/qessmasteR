#' @title  desc_quanti
#' @author Julio Ricardo Davalos
#'
#' @description Permet d'obtenir les principaux indicateurs concernant un groupe de variables quantitatives
#'
#' @param data base de données
#' @param ... variables souhaitées
#' @param moy TRUE par défaut. Moyenne de l'échantillon. Si FALSE, alors pas non plus d'écart-type.
#' @param sd TRUE par défaut. Ecart-type de l'échantillon
#' @param test.norm TRUE par défaut. P-value du test de normalité de l'échantillon (test de Shapiro-Wilk).
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param nb 1 par défaut. Nombre de décimales.
#' @param med TRUE par défaut. Médiane de l'échantillon
#' @param quant 4 par défaut. Nombre de quantiles. Si la médiane est sélectionnée, elle sera ajoutée si besoin.
#' @param minmax TRUE par défaut. Minimum et maximum .
#' @param eff TRUE par défaut. Effectifs.
#' @param eff_na FALSE par défaut. Remplace les effectifs par les effectifs avec les NA. N'apparait pas si eff = FALSE
#' @param NR FALSE par défaut. Garde les non-réponses.
#' @param msg FALSE par défaut. Envoie un message pour chaque variable terminée : utile si bug inexpliqué.
#'
#' @return un data.frame avec les indicateurs sélectionnés pour les variables quantitatives choisies
#'
#' @importFrom dplyr bind_rows mutate %>% filter group_by count rename select pull across summarise
#' @importFrom tidyselect everything
#' @importFrom stringr str_remove str_replace
#' @importFrom purrr map map_dfr map_lgl keep
#' @importFrom DescTools MeanCI
#'
#' @export

desc_quanti <- function(data, ..., moy = TRUE, sd = TRUE, test.norm = TRUE, ic = TRUE, ic_seuil = 0.05, nb = 2, med = TRUE,
                       quant = 4, minmax = TRUE, eff = TRUE, eff_na = FALSE, NR = FALSE, msg = FALSE) {
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
    data_vars <- data_vars %>% keep(is.numeric)
    warning(paste0("Variable(s) ignoree(s) car non-numerique(s) : ", paste(names(list_num)[which(!list_num)], collapse = ", ")))
  }
  # liste des noms :
  list_vars <- map(set_names(names(data_vars)), ~ quo(!!as.name(.x)))

  ######### ELEMENT A GERER PLUS TARD ##########
  # gestion de la ponderation (si vecteur ou non + pb de longueur)
  # if (!is.null(pond)) {# si pas de ponderation alors pas besoin de la normaliser
  #   if (nrow(data_vars) != length(pond)) {
  #     stop("Le vecteur de ponderation n'est pas de la bonne longueur.")
  #   }
  # } else norm_pond <- FALSE

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
    }
    if (eff_na) {# nombre de NA de la variable quanti ou non
      fonc[[length(fonc) + 1]] <- list(NR = function(x) {sum(is.na(x))})
    }
    if (minmax) {
      fonc[[length(fonc) + 1]] <- list(Min = function(x) {min(x, na.rm = TRUE)})
      fonc[[length(fonc) + 1]] <- list(Max = function(x) {max(x, na.rm = TRUE)})
    }
    if (ic) {# d'abord les intervalles de confiance car calcule deja la moyenne
      fonc[[length(fonc) + 1]] <- list(Moyenne = function(x) {
        MeanCI(x, conf.level = (1 - ic_seuil), na.rm = TRUE) %>%
          as.list() %>%
          as.data.frame() %>%
          round(nb) %>%
          rename(Moy = mean, `IC-` = lwr.ci, `IC+` = upr.ci)
      }
      )
    } else if (moy) {# si que moyenne alors calcul avec mean
      fonc[[length(fonc) + 1]] <- list(Moy = function(x) {mean(x, na.rm = TRUE) %>% round(nb)})
    }
    if (sd) {# ecart-type
      fonc[[length(fonc) + 1]] <- list(SD = function(x) {sd(x, na.rm = TRUE) %>% round(nb)})
    }
    if (quant > 0) {# bornes des quantiles
      val_quant <- seq(0, 1, 1 / quant)
      if (med & quant %% 2) { # si mediane et qu'elle est pas contenue dans les quantiles alors on l'ajoute au milieu
        val_quant <- c(val_quant, 0.5)
        val_quant <- val_quant[order(val_quant)]
      }
      if (!med) {# on enleve la mediane
        val_quant <- val_quant[val_quant != .5]
      }
      val_quant <- val_quant[-c(1, length(val_quant))]
      med <- FALSE # pour pas la faire deux fois
      fonc[[length(fonc) + 1]] <- list(Quantiles = function(x) {
        quantile(x, probs = val_quant, na.rm = TRUE) %>%
          as.list() %>%
          as.data.frame() %>%
          rename_with(~str_remove(.x, "X") %>% str_replace("\\.", "%") %>% str_replace("50%", "Med"))
      }
      )
    } else if (med) {# si pas de quantiles mais que quand meme mediane
      fonc[[length(fonc) + 1]] <- list(Mediane = function(x) {quantile(x, probs = 0.5, na.rm = TRUE)})
    }
    if (test.norm) {
      fonc[[length(fonc) + 1]] <- list(`Shapiro-Wilk` = function(x) {
        shap <- shapiro.test(x)$p.value
        return(format(shap, digits = 2, scientific = ifelse(shap < 0.001, T, F)))
        }
      )
    }
    # on a une liste de liste alors qu'on veut qu'une liste
    fonc <- unlist(fonc)

    if (!NR) {
      tab1 <- tab
      # on enleve les NA
      tab <- tab %>% filter(!is.na({{var}}))
        # s'il n'y a que des NA alors pas de lignes
      if (nrow(tab) == 0) {
        warning(paste(nom, " ne contient que des non-reponses, elles sont gardees pour cette variable."), call. = FALSE)
        tab <- tab1
      }
    }
    suppressWarnings(
      # on applique toutes les fonctions selectionnees a la variable quanti
      tab <- tab %>% summarise(map_dfc(fonc, ~exec(.x, {{var}})))
    )
    if (msg) {
      message(paste(nom, "------------------ OK"))
    }
    return(tab)
    }

  # boucle pour toute la liste :
  map_dfr(list_vars, ~desc(!!.x), .id = "Variable")
}
