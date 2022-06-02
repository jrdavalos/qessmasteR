#' @title format_agd
#' @author Julio Ricardo Davalos
#'
#' @description Transforme une base de données préparée pour une analyse géométrique des données afin de supprimer les doublons dans la représentation graphiques. On colle les noms de variables aux modalités si ces dernières sont redondantes avec celles d'autres variables.
#'
#' @param data la base de données
#' @param ttes_var FALSE par défaut. On collerait alors tous les noms de variables à leurs modalités
#'
#' @return Un data.frame de même nature que la base de données utilisée.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate across group_by ungroup add_count filter select case_when distinct cur_column
#' @importFrom tidyselect everything matches

#'
format_agd <- function(data, ttes_var = FALSE) {
  if (ttes_var) {var <- names(data)}
  else {
    var <- data %>%
      pivot_longer(everything(), names_to = "variables", values_to = "modalites") %>%
      group_by(variables, modalites) %>%
      summarise() %>%
      ungroup() %>%
      add_count(modalites) %>%
      filter(n > 1) %>%
      select(variables) %>%
      distinct() %>%
      as.vector()
  }
  data %>% mutate(across(matches(var), ~ case_when(!is.na(.x) ~ paste(cur_column(), .x))))
}
