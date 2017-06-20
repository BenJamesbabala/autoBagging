setMethod("show",
          signature("abmodel"),
          function(object) {
            cat("Ensemble of bagged trees\n")
            cat("No of trees:", length(object@base_models), ".\n")
            cat("Target variable: ", get_target(object@form), ".\n")

            if (object@dynamic_selection == "none") {
              cat("Without dynamic selection.\n")
            } else {
              cat("With dynamic selection method:",
                  object@dynamic_selection, ".\n")
            }
          })

#' Predicting on new data with a \strong{autobagging} model
#'
#' This is a \code{predict} method for predicting new data points using a
#' \code{tseModel} class object - refering to an ensemble for time series
#' forecasting.
#'
#' @seealso \code{\link{abmodel-class}} for details about the ensemble model;
#'
#' @param object A \strong{abmodel-class} object.
#' @param newdata New data to predict using \code{autobagging} model object
#'
#' @return predictions made by an \code{autobagging} model.
#' 
#' @import party
#'
#' @export
setMethod("predict",
          signature("abmodel"),
          function(object, newdata) {

            switch(object@dynamic_selection,
                "ola" = {
                  cat("Using OLA method to dynamically
                       predict new instances...\n")
                  predict.OLA(object@form,
                              object@base_models,
                              object@data,
                              newdata,
                              5)
                  },
                "knora-e" = {
                  cat("Using KNORA-E method to dynamically
                       predict new instances...\n")
                  predict.KNORA.E(object@form,
                                  object@base_models,
                                  object@data,
                                  newdata,
                                  5)
                  },
                "none" = {
                  cat("Using majority voting method to
                       predict new instances...\n")
                  Y_hat <- sapply(object@base_models, predict, newdata)
                  apply(Y_hat, 1, voting)
                  })
          })
