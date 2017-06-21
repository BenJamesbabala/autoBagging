#' autoBagging
#'
#' Learning to Rank Bagging Workflows with Metalearning
#'
#' @param form formula. Currently supporting only categorical target
#' variables (classification tasks)
#' @param data training dataset with a categorical target variable
#'
#' @references Pinto, F. ...
#'
#' @seealso \code{\link{bagging}} for the bagging pipeline with a specific
#' workflow; \code{\link{baggedtrees}} for the bagging implementation;
#' \code{\link{abmodel-class}} for the returning class object
#'
#' @return an \code{abmodel} class object
#'
#' @import xgboost
#'
#' @export
autoBagging <- function(form, data) {
  if (!class(data[, get_target(form)]) %in% c("factor", "character")) {
    stop("autoBagging currently only supports classification tasks.
       Check your target variable in the formula provided.", call. = FALSE)
  }

  if (nrow(data) > 100000L) # QUAL O THRESHOLD?
    warning("Very large datasets are out of the scope
            of the experimental setup
            used to validate autoBagging.
            Check references for further information.",
            call. = FALSE)
  cat("Sit tight, relax and enjoy your coffee. autoBagging is working for you!\n\n")
  cat("https://www.youtube.com/watch?v=hMr3KtYUCcI\n")
  meta.example <- meta.dataframe(data, metafeatures_names)

  for (i in colnames(Xtest)[1:143]) {
    max_value <- MaxMinMetafeatures[MaxMinMetafeatures[, 1] == i, 2]
    min_value <- MaxMinMetafeatures[MaxMinMetafeatures[, 1] == i, 3]
    meta.example[[i]] <-  ifelse(is.finite(normalize01(meta.example[[i]],
                                                       max_value,
                                                       min_value)),
                                 normalize01(meta.example[[i]],
                                             max_value,
                                             min_value), -1)

    Xtest[,i] <- meta.example[[i]]
  }

  X <- xgboost::xgb.DMatrix(data = data.matrix(Xtest),  group = c(63), missing = -1)

  data("metamodel")
  meta.model <- xgboost::xgb.load(metamodel)

  meta.pred <- cbind(as.data.frame(Xtest), predict(meta.model, X))
  colnames(meta.pred)[ncol(meta.pred)] <- c("meta.pred")

  cat("Your recommended workflow is...\n\n")
  RecWF <- cleanRecWF(meta.pred[which.max(meta.pred$meta.pred),
                                (ncol(meta.pred)-13):ncol(meta.pred)])

  catWF(RecWF)
  # acrescentar opcao para treinar o modelo ou simplesmente retornar o melhor set???
  cat('################################\n\n')
  cat('Training recommended workflow...\n\n')

  bagging(form = form,
          data = data,
          ntrees = RecWF$nTrees,
          pruning = RecWF$pruningMethods,
          dselection = RecWF$dynamicMethods,
          pruning_cp = RecWF$pruningCutPoint)
}
