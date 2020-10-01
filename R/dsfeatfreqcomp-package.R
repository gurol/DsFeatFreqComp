#' @author Gürol Canbek, <gurol44@gmail.com>
#' @references G. Canbek, (2020) Gaining Insights in Datasets in the Shade of “Garbage In, Garbage Out” Rationale: Feature-Space Distribution Fitting
#' (\href{http://gurol.canbek.com/Publications/}{Publications by Gurol Canbek})
#' @keywords dataset analysis, quantitative analysis, feature engineering
#' @title DsFeatFreqComp – Dataset Feature-Frequency Comparison Package
#' @description Visualization of the comparisons among a group of datasets or
#' between the datasets in pairs. Especially, representing p-values.
#'
#' The dsfeatfreqcomp package provides two categories of important functions:
#' dataset manipulation and visualization.
#' 
#' @section Dataset manipulation functions:
#' The dataset manipulation functions are
#' \itemize{
#'  \item \code{\link{loadDsFeatFreqsFromCsv2}}
#'  \item \code{\link{meltDataFrame}}
#'  \item \code{\link{loadPairwiseDsComparisonOfMeanRanks}}
#'  \item \code{\link{getPairwiseDsPValueMatrix}}
#' }
#'
#' @section Related visualization functions:
#' \itemize{
#'  \item \code{\link{plotDsFreqDistributionViolin}}
#'  \item \code{\link{plotQQ}}
#'  \item \code{\link{plotPairwiseDsPValuesHeatMap}}
#' }
#'
#' @docType package
#' @name dsfeatfreqcomp
NULL
