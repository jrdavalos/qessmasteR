#' multi_mpl
#'
#' @param var_y variable dépendante, character
#' @param var_x variables explicatives, somme en format character.
#' @param donnees base de données
#' @param poids variable de pondération
#' @param sous_base vecteur optionel spécifiant la sous base des observations à utiliser. Ex : row_number(A$B == 'Oui')
#' @param ... variable de pondération ou sous ensemble
#'
#' @return
#' @export
#'
#' @importFrom stats pnorm
#' @importFrom stats na.omit
#'
#' @examples
#' data(Music)
#' multi_mpl("Age", "Rap + Rock + Jazz + Jazz:Rock", Music)
multi_mpl <- function(var_y, var_x, donnees, poids = NULL, sous_base = NULL,...) {
  #get names of stuff in ...
  argNames = sapply(substitute(list(...))[-1L], deparse)
  # look for identical names in donnes
  m = match(names(donnees), argNames, 0L)
  # store other arguments from ... in a list
  args = list(eval(parse(text = argNames[-m])))
  # name the list
  names(args) = names(argNames[-m])
  # store complete values in args, instead of just references to columns
  # the unlist code is rather ugly, the goal is to create a list where every
  # element is a column of interest
  args[names(argNames)[m]] = unlist(apply(donnees[, as.logical(m), drop = FALSE],
                                          2, list), recursive = FALSE)
  # also put other stuff in there
  args$family = gaussian
  args$data = donnees
  args$weights = poids
  args$subset = sous_base
  args$na.action = na.omit
  args$model = TRUE
  mod_y <- levels(as.factor(donnees[,var_y]))
  nb_mod_y <- length(mod_y)
  regressions <- list(1)
  for (i in 1:nb_mod_y) {
    args$formula <- as.formula(paste(var_y, " == '", mod_y[i],"' ~", var_x, sep = ""))
    regressions[[i]] <- do.call(glm, args)
  }

  tab_coef_seuil <- function(lr, nr) {
    coefs <- t(t(lr[[nr]][[c("coefficients")]])) # récupère la liste des coefficients
    nb_mod_x <- lr[[nr]][["rank"]] # nombre de modalités à coefs
    valeurs_p <- t(t(summary(lr[[nr]])[["coefficients"]][,4]))
    coefs <- as.data.frame(round(coefs*100,2))

    for (i in 1:nb_mod_x) {
      p <- valeurs_p[i,] # On applique le code habituel pour les significativités
      if (p < 0.01) {
        coefs[i,] <- as.character(paste(coefs[i,],"***",sep = ""))
      }
      if (p > 0.01 & p < 0.05) {
        coefs[i,] <- as.character(paste(coefs[i,],"**",sep = " "))
      }
      if (p > 0.05 & p < 0.1) {
        coefs[i,] <- as.character(paste(coefs[i,]," *",sep = " "))
      }
    }
    return(coefs)
  }

  tab_ref <- function(list_reg,nb_reg) {
    nb_var_x <- length(list_reg[[nb_reg]][["xlevels"]]) # nombre de var X
    tab <- tab_coef_seuil(list_reg,nb_reg) # calcul des coefs et tableau avec significativité
    temp <- tab["(Intercept)",]
    l_temp <- 1
    for (i in 1:nb_var_x) { # on met un espace vide entre chaque variable
      nb_mod_var <- length(list_reg[[nb_reg]][["xlevels"]][[i]])
      temp <- rbind(temp, "")
      for (j in 2:nb_mod_var) {
        temp <- rbind(temp,tab[paste(names(list_reg[[nb_reg]][["xlevels"]])[i],
                                     list_reg[[nb_reg]][["xlevels"]][[i]][j], sep = ""),])
        l_temp <- l_temp + 1 # On compte le nombre de valeurs
      }
    }
    mod_x <- "Constante"
    for (i in 1:nb_var_x) { # on crée le vecteur avec les noms de lignes
      nb_mod_x <- length(levels(as.factor(list_reg[[nb_reg]][["xlevels"]][[i]])))
      for (j in 1:nb_mod_x) {
        mod_x <- c(mod_x,
                   paste(names(list_reg[[nb_reg]][["model"]])[i + 1],
                         levels(list_reg[[nb_reg]][["model"]][[names(list_reg[[nb_reg]][["model"]])[i + 1]]])[j], sep = "."))
      }
    }
    if (l_temp < nrow(tab)) { # On ajoute les interactions s'il y en a
      l_temp1 <- l_temp + 1
      temp <- rbind(temp, "", t(t(tab[l_temp1:nrow(tab),])))
      mod_x <- c(mod_x, "Interactions", rownames(tab)[l_temp1:nrow(tab)])
    }
    temp <- rbind(temp, # On ajoute la qualité des modèles
                  "",
                  round(with(list_reg[[nb_reg]], 1 - deviance/null.deviance),2),
                  round(list_reg[[nb_reg]][["aic"]]))
    mod_x <- c(mod_x, "Tests de qualité", "R2","AIC")
    rownames(temp) <- mod_x
    colnames(temp) <- paste(list_reg[[nb_reg]][["terms"]][[2]][[2]],list_reg[[nb_reg]][["terms"]][[2]][[3]], sep = ".")

    tab <- as.data.frame(temp)
    return(tab)
  }

  tableaux <- list(1)
  for (i in 1:nb_mod_y) {
    tableaux[[i]] <- tab_ref(regressions,i)
  }
  tableaux[[nb_mod_y + 1]] <- tableaux[[1]]
  for (i in 2:nb_mod_y) {
    tableaux[[nb_mod_y + 1]] <- as.data.frame(cbind(tableaux[[nb_mod_y + 1]],tableaux[[i]]))
  }

  return(list(regressions = regressions, tableaux = tableaux))
}
