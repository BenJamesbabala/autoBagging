#' majority voting
#'
#' @param x predictions ?????
#'
#' @export
voting <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

normalize01 <- function(x, max_value, min_value) {
	(x - min_value) / (max_value - min_value)
}

#' get target variable
#'
#' get the target variable from a formula
#'
#' @param form formula
#'
#' @export
get_target <- function(form) {
	unlist(strsplit(deparse(form), split = " ", fixed = TRUE), use.names = FALSE)[1]
}

split_by <- function(expr, split, ...) {
  expr <- strsplit(expr, split = split, fixed = FALSE, ...)

  unlist(expr, use.names = FALSE)
}

cleanRecWF <- function(o) {
  exprs <- "nTrees.|pruningMethods.|pruningCutPoint.|dynamicMethods."

  o$meta.pred <- NULL
  o <- as.list(o)
  o_selected <- o[o == 1]
  o_methods <- names(o_selected)

  methods_vals <- lapply(o_methods, function(j) split_by(j, exprs)[2])
  names(methods_vals) <- c("nTrees", "pruningMethods", "pruningCutPoint","dynamicMethods")
  methods_vals$nTrees <- as.numeric(methods_vals$nTrees)

  if (methods_vals$pruningMethods != "none")
    methods_vals$pruningCutPoint <- as.numeric(methods_vals$pruningCutPoint)

  methods_vals
}

catWF <- function(wf) {
  cat("Bagging ensemble of",wf$nTrees,"decision trees.\n\n")
  if (!wf$pruningMethods %in% "none") {
    cat("With a pruning cut of",
        wf$pruningCutPoint,
        "with method",wf$pruningMethods,"\n\n")
  } else {
    cat("Without tree pruning.")
  }

  if (!wf$dynamicMethods %in% "none") {
    cat("Predictions should be produced dynamically with method",
        wf$dynamicMethods,"\n\n")
  } else {
    cat("Predictions should be produced according to majority voting.")
  }
}

#avgWorkflowKNN <- function (avgRankMatrix, dist_metafeatures, k) {
#  k_datasets <- names(sort(dist_metafeatures)[2:(k+1)])
#  normalizedMeanRank <- normalize01(as.data.frame(unlist(dlply(dat[dat$dataset %in% k_datasets,], .(workflow), function(x){mean(x$rankWorkflow)}))))
#  normalizedMeanRank <- normalizedMeanRank[as.integer(dat[dat$dataset == i, c("workflow")]),]
#  return(normalizedMeanRank)
#}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

Log2 <- function(values) sapply(values, log0)

log0 <- function(x) {
  if (x == 0.0 || is.na(x))
    0.0
  else
    log2(x)
}

gaussian.dist <- function(x,y,w)
{
  exp(-sum((x-y)^2)/(2*w^2))
}

constroi.formula<-function(classe,vars) {
  texto<-paste(classe," ~ ",paste(vars,collapse=" + "),sep="")
  form<-eval(parse(text=texto))
  return(form)
}

f.to.one.zero <- function (dat) { cbind(dat[,-c(which(sapply(dat, is.factor)))], model.matrix(eval(parse(text=paste("~"," 0 + ",paste(names(which(sapply(dat, is.factor))),collapse=" + "),sep=""))), model.frame(eval(parse(text=paste("~"," 0 + ",paste(names(which(sapply(dat, is.factor))),collapse=" + "),sep=""))),dat,na.action=function(x) x )) )}

