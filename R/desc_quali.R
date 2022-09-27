#' @title  desc_quali
#' @author Kanto Fiaferana & Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tri à plat de plusieurs variables catégorielles en même temps
#'
#' @param data base de données
#' @param ... variables souhaitées
#' @param eff TRUE par défaut. Fait apparaître les effectifs par catégorie
#' @param freq TRUE par défaut. idem mais avec les fréquences
#' @param cum_freq FALSE par défaut. Fréquences cumulées (veillez bien à ranger les modalités dans l'ordre souhaité)
#' @param NR FALSE par défaut. Les non-réponses comptent-elles parmi les catégories ?
#' @param pond NULL par défaut. Vecteur contenant les poids
#' @param norm_pond TRUE par défaut. Normalise les poids (poids moyen = 1)
#'
#' @return un tableau avec les effectifs et fréquences de chaque modalité de chaque variable sélectionnée
#'
#' @importFrom dplyr bind_rows mutate %>% filter group_by count rename select
#' @importFrom tidyselect everything
#' @importFrom purrr map map_dfr
#'
#' @export

desc_quali <- function(data, ..., eff = TRUE, freq = TRUE, cum_freq = FALSE, NR = FALSE, pond = NULL, norm_pond = TRUE) {
  if (cum_freq) {# si freq cumule alors forcement non cumule
    freq <- TRUE
  }
  # tidyselect des variables a utiliser :
  data_vars <- data %>% select(...)
  if (ncol(data_vars) == 0) {
    warning("Pas de variable selectionnee, utilisation de toutes les variables non numeriques.", call. = FALSE)
    data_vars <- data %>% select(everything() & !where(is.numeric))
  }
  # liste des noms et tri si jamais pas de nom:
  list_vars <- map(set_names(names(data_vars)), ~ quo(!!as.name(.x)))
  # gestion de la ponderation (si vecteur ou non + pb de longueur)
  if (!is.null(pond) & !is.vector(pond)) {
    pond <- data %>% select({{pond}}) %>% unlist(use.names = FALSE)
  } else if (is.vector(pond)) {
    if (length(data_vars[,1]) != length(pond)) {
      stop("Le vecteur de ponderation n'est pas de la bonne longueur.")
    }
  } else if (is.null(pond)) {# si pas de ponderation alors pas besoin de la normaliser
    norm_pond <- FALSE
  }

  # fonction de tri a plat par variable
  desc <- function(var) {
    # on selectionne la variable
    tab <- data %>% select({{var}})
    test <- tab %>% unlist()
    nom <- tab %>% names()
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
      tab <- tab %>% mutate(Frequence = N / sum(N))
    }

    if (cum_freq) {# ajout des frequences cumulees
      tab <- tab %>% mutate(`Frequence cumulee` = cumsum(Frequence))
    }
    if (!eff) {# si pas les effectifs, on les enleve
      tab <- tab %>% select(-N)
    }
    # on renomme la colonne 1 "modalites" et on la met en character
    tab %>% rename(Modalite = 1) %>% mutate(Modalite = as.character(Modalite))
  }

  # boucle pour toute la liste :
  map_dfr(list_vars, ~desc(!!.x), .id = "Variable")
}
