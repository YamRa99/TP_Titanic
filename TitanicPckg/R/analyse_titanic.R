#' Taux de survie en fonction de la classe
#'
#' @param data Data frame contenant les informations sur les passagers
#' @return Un tableau avec le taux de survie pour chaque classe
#' @examples
#' taux_survie_class(df)
#' @export
taux_survie_class <- function(data) {
  library(dplyr)
  return(data %>%
           group_by(passengerClass) %>%
           summarize(TauxSurvie = sum(survived == "yes") / n()))
}


#' Taux de survie en fonction du sexe
#'
#' @author Yamina Sadallah
#' @param data Data frame contenant les informations sur les passagers
#' @return Un tableau avec le taux de survie pour chaque sexe
#' @examples
#' taux_survie_sex(df)
#' @export
taux_survie_sex <- function(data) {
  library(dplyr)
  return(data %>%
           group_by(sex) %>%
           summarize(TauxSurvie = sum(survived == "yes") / n()))
}

