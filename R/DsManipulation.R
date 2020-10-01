# DsFeatFreqComp – Dataset Feature-Frequency Comparison (Dataset Manipulation)
# Copyright (C) 2017-2020 Gürol Canbek
# This file is licensed under
#
#   GNU Affero General Public License v3.0, GNU AGPLv3
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# See the license file in <https://github.com/gurol/dsfeatfreqcompt>
#
#' @author Gürol Canbek, <gurol44@gmail.com>
#' @references G. Canbek, (2020) Gaining Insights in Datasets in the Shade of “Garbage In, Garbage Out” Rationale: Feature-Space Distribution Fitting
#' (\href{http://gurol.canbek.com/Publications/}{Publications by Gurol Canbek})
#' @title DsFeatFreqComp – Dataset Feature-Frequency Comparison (Dataset Manipulation)
#' @description Various dataset manipulation functions used in dataset comparison

#' load dataset feature frequencies from a CSV file
#'
#' Read a CSV (comma seperated values) file having dataset feature frequencies
#' into a dataframe and return it.
#'
#' CSV file should have the following columns and rows:
#' Feature_Name | DSA | DS0 | DS1 | ... | DSN
#' ABC | 0.1 | 0.3 | 0.999 | 0.76 | ... | 0.444
#' ...
#'
#' @param workbook CSV file path
#' @param seperator Seperator character (default: ';')
#'
#' @return data frame having feature name in the first column and feature freq-
#' uencies for each datasets in subsequent columns (DSi)
#' @export
loadDsFeatFreqsFromCsv2<-function(workbook, seperator=';')
{
  df <- as.data.frame(utils::read.delim(file=workbook, sep=seperator))

  return(df)
}

#' Melt a dataframe having all the datasets' feature and frequencies into
#' feature, dataset and frequency
#'
#' Melt (transform) a dataframe into the following structure (transactions) and
#' return it. \code{identity_columns} become \code{variable} and values under
#' each dataset column becomes 'value'.
#'
#' Feature_Name | DSA | DS0 | DS1 | ... | DSN
#' ABC | 0.1 | 0.3 | 0.999 | 0.76 | ... | 0.444
#'
#' into a feature per single dataset per value rows. For example
#'
#' Feature_Name | variable | value
#' ABC | DSA | 0.1
#' ... | DSA | ...
#' ABC | DS0 | 0.3
#' ... | DS0 | ...
#' ABC | DS1 | 0.999
#' ... | DS1 | ...
#' ...
#' ABC | DSN | 0.444
#' ... | DSN | ....
#'
#' @param df Data frame with Feature_name column and feature frequency per
#'   dataset
#' @param identity_columns Column name for the feature name (default first
#'   column's name)
#'
#' @return data frame
#' @export
#'
#' @examples
#' df_melted <- meltDataFrame(df)
#' @note To change original column (dataset) order use \code{df[,c(1, 8, 2:7)]}
#' which moves the last column (i.e. dataset) into the second column.
meltDataFrame<-function(df, identity_columns=names(df)[1])
{
  return(reshape::melt(df, id=identity_columns))
}

#' Load pairwise dataset comparison of mean ranks from a spreadheet file
#'
#' Load pairwise dataset comparison of mean ranks from a spreadsheet file into a
#' data frame having the following column structure:
#'
#' DSi | DSj | PValue
#'
#' that indicates the pair datasets and the p-value of null hypothesis that the
#' pairs come from the similar distribution.
#'
#' These values can be obtained after
#' running the following matlab commands (positive a variable where its values
#' are pasted from dataset feature frequencies as specified in
#' loadDsFeatFreqsFromCsv2 function).
#'
#' \code{[p_p, tbl_p, stats_p] =
#'   kruskalwallis(positive,
#'                 {'DSA', 'DS0', 'DS1', 'DS2', 'DS3', 'DS4', 'DS5'})}
#' \code{c_p=multcompare(stats_p)}
#'
#' workbook input parameter actually an excel sheet having 'c_p' values the
#' followup test result for each pair of datasets with the following column
#' structure without headers in the first row.
#'
#' DSA | DS0 | -75,06246083 | -17,21276596 | 40,63692892 | 0,975982403
#' DSA | DS1 | -47,47735445 | 10,37234043  | 68,2220353  | 0,998441473
#' DSA | DS2 | -51,99863105 | 5,85106383   | 63,70075871 | 0,999943324
#' DSA | DS3 | -61,78586509 | -3,936170213 | 53,91352466 | 0,99999458
#' DSA | DS4 | -62,94543956 | -5,095744681 | 52,7539502  | 0,999974927
#' DSA | DS5 | -62,87097147 | -5,021276596 | 52,82841828 | 0,999977018
#' DS0 | DS1 | -30,26458849 | 27,58510638  | 85,43480126 | 0,798939376
#' ...
#'
#' Note that the first two columns are actually stated as index of the datasets
#' stated in the \code{kruskalwallis} matlab function shown above (DSA: 1, DS0:
#' 2, ...) Thus, these index values should be changed by corresponding dataset
#' name in the excel workbook.
#'
#' @param workbook Excel workbook path
#'
#' @return multi comparison data frame with 'DSi', 'DSj', 'PValue' columns
#' @export
loadPairwiseDsComparisonOfMeanRanks<-function(workbook)
{
  df_multicompare <-
    readxl::read_excel(workbook, col_names=FALSE,
               col_types =
                 c('text', 'text', 'skip', 'skip', 'skip', 'numeric'))
  names(df_multicompare) <- c('DSi', 'DSj', 'PValue')
  return(df_multicompare)
}

#' Get pairwise dataset P value matrix from multicomparison p-values
#'
#' Convert multi comparison data frame having 'DSi', 'DSj', 'PValue' columns
#' into a two dimensional matrix around PValue that can be used for
#' \code{\link{plotPairwiseDsPValuesHeatMap}} function
#'
#' @param df_multicompare data frame having 'DSi', 'DSj', 'PValue' columns
#'
#' @return matrix
#' @export
#'
#' @examples
#' data(dscompmeanrankspositive)
#' mat_pairwise_ds_pvalues <- getPairwiseDsPValueMatrix(dscompmeanrankspositive)
getPairwiseDsPValueMatrix<-function(df_multicompare)
{
  mat2df <- reshape::cast(df_multicompare, DSi~DSj, value='PValue')
  mat_pairwise_ds_pvalues <- as.matrix(mat2df)
  return(mat_pairwise_ds_pvalues)
}

#' Return the intersection of feature names given as a comma seperated list per dataset (i.e. Copy)
#'
#' Example usage (from spread sheet)
#' 1) Open sample datasets.ods file in https://github.com/gurol/dsfeatfreqdist
#' 2) Select Features (Permissions) cells in dsdist (fit features) worksheet  
#'      for the following filters  
#'         "Malign" in Class and  
#'         1 in Plausibility Degree and  
#'         "Fitted (>=xmin)" in Type and  
#'         exclude "NA" rows  
#'      Don't forget to include the first header row cell  
#'         "Features (Permissions)" into your selection  
#' 3) Copy the selected cells and set them to a data frame with one column 
#' 4) Call getCommonFeatures() function
#'
#' @param ds_fit_unfit_features data frame having comma seperated list of fit/unfit features
#' @param split seperator between column values (default: ', ')  
#'  
#' @return character vector  
#' @export
#'
#' @examples
#' X <- data.frame(features = c('x1, x2, x3', 'x1, x2'), stringsAsFactors = FALSE)
#' getCommonFeatures(X)
getCommonFeatures<-function(ds_fit_unfit_features, split=', ')
{
  common_features <- character()
  ds_count <- nrow(ds_fit_unfit_features)
  
  if (ds_count == 0) {
    return(common_features)
  }
  
  common_features <- strsplit(ds_fit_unfit_features[1, 1], split=split)[[1]]
  
  for (i in 1:(ds_count-1)) {
    features <- strsplit(ds_fit_unfit_features[i+1, 1], split=split)[[1]]
    common_features <- intersect(common_features, features)
  }
  
  return(common_features)
}
