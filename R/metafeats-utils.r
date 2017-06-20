#' Retrieve the value of a previously computed measure
#'
#' @param inDCName name of data characteristics
#' @param inDCSet set of data characteristics already computed
#' @param component.name name of component (e.g. time or value) to
#' retrieve; if NULL retrieve all
#'
#' @return simple or structured value
#'
#' @note if measure is not available, stop execution with error
GetMeasure <- function(inDCName, inDCSet, component.name = "value") {
  if (is.null(inDCSet$value[[inDCName]]))
    stop(message = "WARNING:requires uncomputed measure (", inDCName, ")")

  if (is.null(component.name))
    inDCSet[[inDCName]]
  else
    inDCSet[[component.name]][[inDCName]]
}

#' Retrieve names of symbolic attributes (not including the target)
#'
#' @param dataset structure describing the data set, according
#' to \code{read_data.R}
#'
#' @seealso read_data.R
#'
#' @return list of strings
SymbAttrs <- function(dataset) {
  dataset[[1]]$attributes$attr.name[(dataset[[1]]$attributes$attr.type != "continuous") & (dataset[[1]]$attributes$attr.name != dataset[[1]]$attributes$target.attr)]
}

#' Retrieve names of continuous attributes (not including the target)
#'
#' @inheritParams SymbAttrs
#'
#' @seealso read_data.R
#'
#' @return list of strings
ContAttrs <- function(dataset) {
  dataset[[1]]$attributes$attr.name[(dataset[[1]]$attributes$attr.type == "continuous") & (dataset[[1]]$attributes$attr.name != dataset[[1]]$attributes$target.attr)]
}

#' FUNCTION TO TRANSFORM DATA FRAME INTO LIST WITH GSI REQUIREMENTS
#'
#' @param dat data frame
#'
#' @return a list containing components that describe
#' the names (see ReadtAttrsInfo) and the data (see ReadData) files
#'
#' @note what about regression?
#' THIS FUNCTION HAS TO BE BASED IN READATTRSINFO AND READDATA
ReadDF <- function(dat) {
  # Determine attribute types
  wkNamesFile <- lapply(dat, function(attr)
  {
    if (is.numeric(attr))
    {
      "continuous"
    }
    else if (is.factor(attr))
    {
      levels(attr)
    }
    else
    {
      "error"
    }
  })

  # Save .names file
  names(wkNamesFile)[length(wkNamesFile)] <- c("class")

  wkDataset <- alist()
  class(wkDataset) <- "dataset" # why is this necessary?

  target.attr <- names(wkNamesFile)[length(wkNamesFile)]
  original.attr.name <- names(wkNamesFile)
  attr.name <- names(wkNamesFile)
  attr.type <- wkNamesFile
  problem.type <- "classification"

  wkNamesFile <- list(namesfile=c("datafile"), attributes=list(target.attr = target.attr, problem.type = problem.type, attr.name = attr.name, original.name = original.attr.name, attr.type = attr.type))
  frame <- dat
  colnames(frame) <- original.attr.name
  rownames(frame) <- NULL

  return(list(wkNamesFile, list(data.file = "datafile", frame = frame)))
}


CharacterizeDF <- function(df, dc.measures) {

  wkDCSet <- list(value = list())
  wkDataSet <- ReadDF(df)

  for (wkMeasure in dc.measures$measures)
  {
    if (is.null(wkDCSet$value[[wkMeasure]]))
    {
      wkValue <- do.call(wkMeasure, list(wkDataSet, wkDCSet))
      wkDCSet$value[[wkMeasure]] <- wkValue
    }
  }
  return(wkDCSet)
}

meta.dataframe <- function (dat, metaf) {

  CDF <- CharacterizeDF(dat, kCompleteClassificationGSI)

  metaframe <- NULL

  for (i in 1:length(metaf)) {
    metaframe[i] <- CDF$value[grep(metaf[[i]], names(CDF$value))][1]
  }
  names(metaframe) <- metaf
  return(metaframe)
}

