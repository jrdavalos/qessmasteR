#' multi_mpl
#'
#' @param formula formule de la régression à appliquer
#' @param data base de données
#' @param subset optionnel, sous base des observations à utiliser
#' @param weights variable de pondération
#' @param na.action ce qui se passe en cas de présence de NA. Voir la documentation de lm
#' @param method méthode utilisée, "qr" est la valeur par défaut, voir la documentation de lm
#' @param model logique. Retourne les arguments si TRUE
#' @param x logique. Retourne les arguments si TRUE
#' @param y logique. Retourne les arguments si TRUE
#' @param qr logique. Retourne les arguments si TRUE
#' @param singular.ok logique. voir lm
#' @param offset arguments additionnels (voir lm)
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
#' multi_mpl(Age ~ Rap + Rock + Jazz + Jazz:Rock, Music)

multi_mpl <- function(formula, data, subset = NULL, weights = NULL, na.action = na.omit, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, offset = NULL,...) {
  argNames = sapply(substitute(list(...))[-1L], deparse)
  # look for identical names in donnes
  m = match(names(data), argNames, 0L)
  # store other arguments from ... in a list
  args = list(eval(parse(text = argNames[-m])))
  # name the list
  names(args) = names(argNames[-m])
  # store complete values in args, instead of just references to columns
  # the unlist code is rather ugly, the goal is to create a list where every
  # element is a column of interest
  args[names(argNames)[m]] = unlist(apply(data[, as.logical(m), drop = FALSE],
                                          2, list), recursive = FALSE)
  # also put other stuff in there
  args$data = data
  args$weights = weights
  args$subset = subset
  args$na.action = na.action
  args$method = method
  args$model = model
  args$x = x
  args$y = y
  args$qr = qr
  args$singular.ok = singular.ok
  args$offset = offset

  mod_y <- levels(unlist(c(data[as.character(formula[[2]])])))
  reg <- list()
  for (i in mod_y) {
    args$formula <- update.formula(formula,as.formula(paste0(formula[[2]],"=='",i,"'~ .")))
    reg[[i]] <- do.call(lm, args)
  }
  reg
}
