#' abmodel-class
#'
#' \strong{abmodel} is an S4 class that contains the ensemble model.
#' Besides the base learning algorithms--\code{base_models} --
#' \strong{abmodel} class contains information about the
#' dynamic selection method to apply in new data.
#'
#' @slot base_models list comprising the tree learning models.
#'
#' @slot form formula
#' @slot data training data for the tuning of the dynamic selection methods.
#'
#' @slot dynamic_selection Dynamic selection method to apply in new data. Character.
#' This value is predicted by the metalearning model.
#'
#' @seealso \code{\link{autoBagging}} function for the method os predicting the
#' best workflow.
#'
#' @export
setClass("abmodel",
         slots = c(base_models = "list",
                   form = "formula",
                   data = "data.frame",
                   dynamic_selection = "character")
)

#' abmodel
#'
#' @param base_models adsd
#' @param form formula
#' @param data data
#' @param dynamic_selection asdada
#'
#' @export
abmodel <- function(base_models, form, data, dynamic_selection) {
  if ( !dynamic_selection %in%  c("ola", "knora-e", "none"))
  	stop("Please choose a valid dynamic selection method", call. = FALSE)

  new("abmodel",
      base_models = base_models,
      form = form,
      data = data,
      dynamic_selection = dynamic_selection)
}
