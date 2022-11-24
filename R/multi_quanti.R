#' @title multi_quanti
#' @author  Julio Ricardo Davalos
#'
#' @description Permet d'obtenir un tableau détaillant une variable quantitative en fonction d'une liste de variables catégorielles.
#'
#' @param data base de données
#' @param var_princ variable principale (quantitative, en colonnes)
#' @param ... variables catégorielles à croiser avec la variable principale (en lignes)
#' @param moy TRUE par défaut. Moyenne par modalité.
#' @param test.diffmoy TRUE par défaut. Réalise un ANOVA et retourne la p-value du test d'égalité des moyennes adapté.
#' La procédure est la suivante: on réalise une ANOVA et un test de Shapiro-Wilk (normalité) sur celle-ci.
#' Si la normalité des résidus est rejetée alors le résultat final est un test de Kruskal-Wallis (on aura alors P(K)).
#' Si elle ne l'est pas alors on réalise un test de Levene (homoscédasticité).
#' Si l'homoscédasticité est rejetée alors le résultat final est un ANOVA de Welch (on aura alors P(W)), sinon on conserve l'ANOVA (on aura alors P(A)).
#' @param force_anova Permet de forcer la fonction à considérer la distribution comme normale. Vecteur des noms des variables catégorielles pour lesquelles forcer.
#' Peut être utile si on considère que la distribution est malgré tout suffisamment proche d'une distribution normale pour ne pas avoir d'incidence sur l'analyse.
#' Peut prendre la valeur TRUE si on veut forcer pour toutes les variables.
#' @param ic_test risque de première espèce des tests de d'égalité des moyennes.
#' @param sd TRUE par défaut. Écart-type par modalité.
#' @param ic TRUE par défaut. Intervalle de confiance de la moyenne par modalité. N'apparait pas si moy = FALSE
#' @param ic_seuil risque de première espèce pour l'intervalle de confiance.
#' @param nb nombre de décimales pour la moyenne, l'écart-type et l'intervalle de confiance.
#' @param med TRUE par défaut. Médiane par modalité.
#' @param quant 4 par défaut. Nombre de quantiles. Si la médiane est sélectionnée, elle sera ajoutée si besoin.
#' @param minmax TRUE par défaut. Minimum et maximum par modalité.
#' @param eff TRUE par défaut. Effectifs par modalité.
#' @param eff_na FALSE par défaut. Effectifs des non-réponses dans la variable quantitative par modalité.
#' @param NR FALSE par défaut. Garde les non-réponses des variables catégorielles.
#' @param msg FALSE par défaut. Envoie un message pour chaque variable terminée : utile si bug inexpliqué.
#'
#' @return Un tibble avec en colonne les indicateurs synthétiques de la variable d'intérêt selon les modalités des variables catégorielles choisies.
#' @export
#'
#' @importFrom stats quantile aov
#' @importFrom rlang set_names quo
#' @importFrom stringr str_remove str_replace
#' @importFrom purrr map_dfr map2_dfc
#' @importFrom dplyr %>% group_by summarise select rename filter n pull rename_with
#' @importFrom DescTools MeanCI
#' @importFrom car leveneTest

multi_quanti <- function(data, var_princ, ..., moy = TRUE, test.diffmoy = TRUE, force_anova = NULL, ic_test = 0.05, sd = TRUE,
                         ic = TRUE, ic_seuil = 0.05, nb = 2, med = TRUE, quant = 4, minmax = TRUE, eff = TRUE, eff_na = FALSE, NR = FALSE, msg = FALSE) {
  if (!moy) {
    ic <- FALSE
    sd <- FALSE
  }
  if (quant < 0) {
    quant <- 0
    warning("Pas de quantile calcule car valeur negative.")
  }
  nom_princ <- data %>% select({{var_princ}}) %>% names()
  sommaire <- function(var) {
    test <- data %>% select({{var}}) %>% pull()
    nom <- data %>% select({{var}}) %>% names()
    if (is.numeric(test)) {
      warning(paste("La variable" , nom, "n'est pas categorielle mais numerique !\nTransformee en categorielle."), call. = FALSE)
    }
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
    # on a une liste de liste alors qu'on veut qu'une liste
    fonc <- unlist(fonc)

    # on enleve ou non les NA de la variable quali
    tab <- data
    if (!NR) {
      tab1 <- tab
      tab <- tab %>% filter(!is.na({{var}}))
      if (nrow(tab) == 0) {
        warning(paste(nom, " ne contient que des non-reponses, elles sont gardees pour cette variable."), call. = FALSE)
        tab <- tab1
      }
    }

    # on cree le tableau
    suppressWarnings(
    tab <- tab %>%
      # par categorie
      group_by({{var}}) %>%
      # on applique toutes les fonctions selectionnees a la variable quanti
      summarise(map_dfc(fonc, ~exec(.x, {{var_princ}})))
    )

    # anova = cas particulier car pas de tri en fonction de la modalite
    if (test.diffmoy) {
      # on cree l'ANOVA
      f <- as.formula(paste0(nom_princ, "~", nom))
      ano <- aov(f, data = data)
      # les residus ont-ils une distribution normale ?
      normal <- shapiro.test(residuals(ano))$p.value

      if (normal < ic_test & !(nom %in% force_anova)) {# cas ou les residus ne suivent pas une loi normale
        # test de Kruskal-Wallis
        pval.test <- stats::kruskal.test(f, data = data)$p.value
        quel.test <- "P(K) ="
        message(paste("Les groupes constitues par la variable", nom,
                      "ne semblent pas avoir une distribution normale. On realise un test de Kruskal-Wallis."))
      } else {
        # on doit savoir si les donnees sont homoscedastiques
        suppressWarnings(homoscedas <- leveneTest(f, data = data)[1, "Pr(>F)"])

        if (homoscedas < ic_test) {# cas ou le modele n'est pas homoscedastique
          # ANOVA de Welch (variances differentes selon les groupes)
          pval.test <- stats::oneway.test(f, data = data)$p.value
          quel.test <- "P(W) ="
          message(paste("Les groupes constitues par la variable", nom,
                        "ne semblent pas avoir une variance homogene. On realise une ANOVA de Welch."))
        } else {
          # dans le dernier cas, on garde l'ANOVA
          pval.test <- summary(ano)[[1]][1, "Pr(>F)"]
          quel.test <- "P(A) ="
        }
      }

      tab <- tab %>%
        mutate(Test = paste(quel.test, format(pval.test, digits = 2, scientific = ifelse(pval.test < 0.001, T, F))))
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

    tab %>%
      rename(Modalite = 1) %>%
      mutate(Modalite = as.character(Modalite))

  }
  # d'abord le dataframe avec uniquement les variables souhaitées :
  data_vars <- data %>% select(...)
  # on peut faire la liste des NOMS comme suit :
  list_vars <- map(set_names(names(data_vars)), ~ quo(!!as.name(.x)))
  if (length(list_vars) == 0) {
    stop(paste("Pas de variable a croiser avec", deparse(substitute(var_princ))))
  }
  # on voit si force_anova = TRUE
  if (force_anova) {
    force_anova <- names(data_vars)
  }
  # on applique nos fonctions à la list :
  map_dfr(list_vars, ~sommaire(!!.x), .id = "Variable")
}


