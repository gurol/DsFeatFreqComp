# DsFeatFreqComp – Dataset Feature-Frequency Comparison
# Copyright (C) 2017-2018 Gürol Canbek
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
#' @references G. Canbek et al. (2018) Gaining New Insight in Datasets via Multiple Binary-Feature Frequency Ranks with A Benign/Malign Mobile Apps Example, Journal of Machine Learning Research (JMLR) (Submitted)
#' (\href{http://gurol.canbek.com/Publications/}{Publications by Gurol Canbek})
#' @keywords dataset analysis, quantitative analysis, feature engineering
#' @title DsFeatFreqComp – Dataset Feature-Frequency Comparison (Visualization)
#' @description Visualization of the comparisons among a group of datasets or
#' between the datasets in pairs. Especially, representing p-values.
#' @note version history
#' 1.1, 16 Nov 2018, Batch processing
#' 1.0, 21 Jun 2018, The first version

#' libraries
# readr: for read.delim
# reshape: melting data frame consisting of Dataset columns into melted
# ggpubr: plotting violin graphs
# gridExtra: ggpubr plots side-by-side
# grid: grobTree in violin graps (old: grid.grob in gList=
# ggsci: scientific colors for creating dataset color palette (simpsons)
# stats: for Shapiro-Wilk normality test in plotQQ
# gplots: heatmap.2
# readxl: for read_excel
# plot3D: for jet.col
# dendextend: set function in heatmap.2
#' @importFrom magrittr %>%
#' @importFrom stats dist hclust as.dendrogram

# source('utilsProxy.R')
# source('DsManipulation.R')

# Globals for functions

# Colors for Positive/Negative Classes and Class Neutral and None
class.colors <- c('#C6EFCE', '#006100', # Green
                  '#FFC7CE', '#9C0006', # Red
                  '#FFEB9C', '#9C5700', # Orange
                  'gray', 'black'       # Black
                 )
names(class.colors) <- c('Negative Light', 'Negative Dark', # Negative class
                         'Positive Light', 'Positive Dark', # Positive class
                         'Neutral Light', 'Neutral Dark',   # Neutral class
                         'None Light', 'None Dark'          # None class
                         )

#' Example colors for datasets
dataset.colors <- ggsci::pal_simpsons()(9)
# Swap colors to comply DSA, DS0, DS1, ... order
dataset.colors[c(2,3)] <- dataset.colors[c(3,2)]
dataset.colors[c(1,2)] <- dataset.colors[c(2,1)]
# Old palatte
# dataset.colors <- c('#E4C83D', '#839099', '#913CCD',
#                     '#4E78D5', '#2CA8C2', '#F86C3C', 'white')

#' Breaks for p-values
#' Narrowed left colors original implementation by Gurol Canbek
#' Copyright (C) 2017-2018 Gürol Canbek
pvalues.breaks <- c(0, 0.001, 0.01, 0.05, seq(0.055, 1, 0.005))
#' Colors for p-values
#' Narrowed left colors original implementation by Gurol Canbek
#' Copyright (C) 2017-2018 Gürol Canbek
#' c('#C6EFCE', '#006100', # Green
#'   '#FFC7CE', '#9C0006', # Red
#'   '#FFEB9C', '#9C5700', # Orange
#'   'gray', 'black'       # Black
#' )
pvalue.colors <- c(rev(plot3D::jet.col(9, alpha=0.66))[1:3],
                   rev(plot3D::jet.col(297, alpha=0.66))[108:297])

#' plot dataset frequency distribution as a violin graph
#'
#' Plot feature frequency statistics per dataset as a violin graph (frequency
#' probability densities) and descriptive statistics: ranges (min/max values
#' shown in vertical line ends), quartiles (lower and upper shown in bottom and
#' top edges of box), medians (horizontal line in box), means (black dot), and
#' outliers (pink dot) per dataset.
#'
#' @param df_melted Data frame melted as Feature_Name | variable (DSi) | value
#'   (feature frequency for DSi)
#' @param condition Datasets' class (default: \code{'Positive'} or
#'   \code{'Negative'})
#' @param ds_palette Dataset colors (default: \code{dataset.colors})
#' @param label_x X axis label (default: \code{'Datasets'})
#' @param label_y Y axis label (default: \code{'Feature frequency probability
#'   densities, ranges, quartiles, means, outliers'})
#'
#' @return None
#' @export
#'
#' @examples
#' data(dsfeatfreqpositive)
#' df_melted <- meltDataFrame(dsfeatfreqpositive)
#' plotDsFreqDistributionViolin(df_melted)
#' @note Development notes:
#' @seealso http://www.sthda.com/english/rpkgs/ggpubr/
#' For pairwise comparison add
#' \code{my_comparisons <- list(c('DS0', 'DSA'), c('DS0', 'DS1'),
#'                              c('DS0', 'DS2'))}
#' Add
#' \code{ggpubr::stat_compare_means(comparisons=my_comparisons, label = 'p.signif')+}
plotDsFreqDistributionViolin<-function(
  df_melted, condition='Positive', ds_palette=dataset.colors, label_x='Datasets',
  label_y='Feature frequency probability densities, ranges, quartiles, means, outliers')
{
  color_dark <- class.colors[paste0(condition, ' Dark')]
  # grob <- grobTree(textGrob('DSA comparisons:', x=0.1, y=0.9, hjust=0,
  #                          gp=gpar(col='gray', fontsize=8)))
  ggpubr::ggviolin(df_melted, x = 'variable', y = 'value', fill = 'variable',
           add=c('boxplot', 'mean'), add.params=list(fill = 'white'),
           palette=ds_palette, xlab=label_x, ylab=label_y,
           ggtheme=ggplot2::theme(
             axis.line=ggplot2::element_line(colour=color_dark),
             panel.background=ggplot2::element_blank(),
             legend.position='none'), legend.title=NULL)+
    ggplot2::geom_boxplot(outlier.colour = 'magenta', fill=grDevices::rgb(0,0,0,alpha=0),
                 color=grDevices::rgb(0,0,0,alpha=0), outlier.size=0.75)+
    ggpubr::stat_compare_means(label.y=1.3) # +
    # Not consistent with Kruskal-Wallis test?
    # ggpubr::stat_compare_means(label = 'p.format', method = 'wilcox.test',
    #                    ref.group = 'DSA', label.y = -0.47, paired=TRUE)+
    # # annotation_custom(grob)+
    # ggpubr::stat_compare_means(label = 'p.format', method = 'wilcox.test',
    #                    ref.group = 'DS0', label.y = -0.40, paired=TRUE)
}

#' Plot Quantile-Quantile chart for all the dataset in one figure
#'
#' Plot Quantile-Quantile chart with Shapiro-Wilk test values and P-values are
#' provided for each dataset. If some of the frequency values (points) are
#' outside the corresponding normal distribution indicated by a shaded area it
#' means that the dataset does not have a normal distribution. All the datasets
#' are plotted into side-by-side in two columns
#'
#' @param df Data frame with Feature_name column and feature frequency per
#'   dataset
#' @param condition Datasets' class (default: \code{'Positive'} or
#'   \code{'Negative'})
#' @param ds_palette Dataset colors (default: \code{dataset.colors})
#' @param skip_column Number of columns skipped (default: 1 [feature name
#'   column])
#' @param ds_names Name of the datasets (default: \code{NULL} [retrieve from
#'   dataframe])
#' @param label_x X axis label (default: \code{FALSE} [Add Shapiro normality test])
#'
#' @return none
#' @export
#'
#' @examples
#' library(dsfeatfreqcomp)
#' 
#' # Colors for datasets
#' dataset.colors <- ggsci::pal_simpsons()(9)
#' # Swap colors to comply DSA, DS0, DS1, ... order
#' dataset.colors[c(2,3)] <- dataset.colors[c(3,2)]
#' dataset.colors[c(1,2)] <- dataset.colors[c(2,1)]
#'
#' data(dsfeatfreqnegative)
#' # There is no negative sample for DS5 arrange colors
#' plotQQ(dsfeatfreqnegative, 'Negative', ds_palette=dataset.colors[c(1:5, 7)])
#'
#' data(dsfeatfreqpositive)
#' # For single dataset plot only (y label is normally 'Sample'
#' # x label is 'Theoretical' in single ggqqplot)
#' plotQQ(as.data.frame(dsfeatfreqpositive[,2:2]), skip_column= 0, ds_names = 'DS2',
#' label_x='Theoretical')
#' @seealso \url{http://www.sthda.com/english/rpkgs/ggpubr/reference/ggqqplot.html}
plotQQ<-function(df, condition='Positive', ds_palette=dataset.colors,
                 skip_column=1, ds_names=NULL, label_x=FALSE)
{
  color_dark <- class.colors[paste0(condition, ' Dark')]
  # plots <- gList()
  plots <- list()
  if (is.null(ds_names)) {
    ds_names <- names(df)
  }
  ds_count <- ncol(df)-skip_column
  label_x2 <- label_x
  for(i in 1:ds_count) {
    if (label_x == FALSE) {
      text <- stats::shapiro.test(df[[skip_column+i]])
      # label_x2 <- paste0('Theoretical (W= ', round(text$statistic, 4),
      label_x2 <- paste0('(W= ', round(text$statistic, 4),
                         ', p-value= ',
                         unname(formatC(text$p.value, format='e', digits=2)),
                         ')')
    }
    plots[[i]] <- ggpubr::ggqqplot(df[[skip_column+i]], color=ds_palette[i],
                           xlab=label_x2, ylab=ds_names[skip_column+i],
                           ggtheme=ggplot2::theme(
                             axis.line=ggplot2::element_line(colour=color_dark),
                             panel.background=ggplot2::element_blank()))
  }
  if (ds_count==1) {
    gridExtra::grid.arrange(grobs=plots, ncol=1)
  }
  else {
    gridExtra::grid.arrange(grobs=plots, ncol=2)
  }
}

#' Plot pairwise dataset p-values as a heatmap diagram
#'
#' Plot a heatmap from two dimensional p-value matrix for datasets. The colormap
#' is originally arranged to indicate the rejection/acceptance limits for null
#' hypothesis stating the similarity:
#'
#' Reject: 0, 0.001, 0.01, 0.05; Failed to Reject: >> 0.05
#'
#' @param mat_pairwise_ds_pvalues Pairwise dataset P values
#'
#' @return None
#' @export
#'
#' @examples
#' data(dscompmeanrankspositive)
#' mat_pairwise_ds_pvalues <- getPairwiseDsPValueMatrix(dscompmeanrankspositive)
#' plotPairwiseDsPValuesHeatMap(mat_pairwise_ds_pvalues)
#' @note Some resources:
#' \url{https://stackoverflow.com/questions/24919413/selecting-number-of-leaf-nodes-of-dendrogram-in-heatmap-2-in-r}
#' \url{https://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}
plotPairwiseDsPValuesHeatMap<-function(mat_pairwise_ds_pvalues)
{
  # heatmap.2(mat_pairwise_ds_pvalues, col='heat.colors', scale = 'none')
  # heatmap.2(mat_pairwise_ds_pvalues, col='bluered', scale = 'none')
  # heatmap.2(mat_pairwise_ds_pvalues, col=rev(jet.col(128)), scale = 'none',
  #          breaks=seq(0, 1, 1/128))

  # Test 1: Equal colormap distribution
  # pvalues.breaks <- c(0, 0.001, 0.01, 0.05, seq(0.055, 1, 0.005))
  # heatmap.2(mat_pairwise_ds_pvalues,
  #           col=rev(jet.col(length(pvalues.breaks)-1)),
  #           scale = 'none', breaks=pvalues.breaks,
  #           margins=c(5,5), cexRow=0.7, cexCol=0.7,
  #           trace = 'none')

  # Test 2: 0.05 is red:
  # pvalues.breaks <- c(0, 0.001, 0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 1)
  # heatmap.2(mat_pairwise_ds_pvalues, col=rev(jet.col(8)),
  #           scale = 'none', breaks=pvalues.breaks)

  # set('branches_k_color', k=2, value=c('red', 'blue'))
  Rowv  <- mat_pairwise_ds_pvalues %>% dist %>% hclust %>%
    as.dendrogram %>%
    dendextend::set('branches_k_color', k=3) %>% dendextend::set('branches_lwd', 2) %>%
    dendextend::rotate_DendSer(ser_weight=dist(mat_pairwise_ds_pvalues))
  Colv  <- mat_pairwise_ds_pvalues %>% t %>% dist %>% hclust %>%
    as.dendrogram %>%
    dendextend::set('branches_k_color', k=3) %>% dendextend::set('branches_lwd', 2) %>%
    dendextend::rotate_DendSer(ser_weight=dist(t(mat_pairwise_ds_pvalues)))

  # Alpha 0.9
  # col.note <- 'gray85'
  # col.trace <- 'gray95'
  # col.line <- 'black'
  # col.dens <- 'gray20'

  # Alpha 0.66
  col.note <- 'black' # p-value text color
  col.trace <- 'gray15' # Vertical p-value in cells
  col.line <- 'gray85' # Dendogram horizontal/vertical projections
  col.dens <- 'gray25' # Density graph p-value/count vertical lines

  gplots::heatmap.2(mat_pairwise_ds_pvalues, col=pvalue.colors,
            scale = 'none', breaks=pvalues.breaks,
            notecol = col.note, tracecol = col.trace, linecol = col.line,
            denscol = col.dens,
            cellnote=round(mat_pairwise_ds_pvalues, 4),
            Rowv=Rowv, Colv=Colv, key.title = 'p-value colormap and histogram',
            keysize=1.75,
            key.xlab = 'Reject: 0, 0.001, 0.01, 0.05; >> Failed to reject',
            notecex=1.25,
            # key.ylab = '',
            # key.ytickfun = function() {list(labels=FALSE, tick=FALSE)},
            # key.par=list(cex=0.6),
            key.xtickfun=
              function() {
                cex <- graphics::par('cex')*graphics::par('cex.axis')
                side <- 1 #? 1=bottom, 2=left, 3=top, 4=right
                line <- 0
                col <- graphics::par('col.axis')
                font <- graphics::par('font.axis')
                graphics::mtext('Reject', side=side, at=0, adj=0,
                      line=line+0.25, cex=cex*0.75, col=col, font=font)
                graphics::mtext('Failed to reject H0', side=side, at=1, adj=1,
                      line=line+0.25, cex=cex*0.75, col=col, font=font)
                return(list(labels=TRUE, tick=TRUE))
              }
            )
}

# Test (Demonstration) Functions ------------------------------------------

#' Test comparison of positive class datasets
#'
#' Test (demonstration) for plotting Quantile-Quantile chart and Kruskal-Wallis
#' Violin chart for positive class
#'
#' @param csv CSV file path
#'
#' @return None
#' @export
#'
#' @examples
#' # testComparePositiveDsFeatFreq(csv='positive.csv')
testComparePositiveDsFeatFreq<-function(csv='Datasets/Positive_SeperatorSemiColon.csv')
{
  df <- loadDsFeatFreqsFromCsv2(csv)
  df <- df[,c(1, ncol(df), 2:(ncol(df)-1))]
  testCompareDsFeatFreq(df, 'Positive', palette=dataset.colors)
}

#' Test comparison of negative class datasets
#'
#' Test (demonstration) for plotting Quantile-Quantile chart and Kruskal-Wallis
#' Violin chart for negative class
#'
#' @param csv CSV file path
#'
#' @return None
#' @export
#'
#' @examples
#' # testCompareNegativeDsFeatFreq(csv='negative.csv')
testCompareNegativeDsFeatFreq<-function(csv='Datasets/Negative_SeperatorSemiColon.csv')
{
  df <- loadDsFeatFreqsFromCsv2(csv)
  df <- df[,c(1, ncol(df), 2:(ncol(df)-1))]
  testCompareDsFeatFreq(df, 'Negative', palette=dataset.colors[c(1:5, 7)])
}

#' Test comparison of datasets
#'
#' Test (demonstration) for plotting Quantile-Quantile chart and Kruskal-Wallis
#' Violin chart for positive/negative class
#'
#' @param df Data frame with Feature_name column and feature frequency per
#'   dataset
#' @param condition Datasets' class (default: \code{'Positive'} or
#'   \code{'Negative'})
#' @param palette Dataset colors (default: \code{dataset.colors})
#'
#' @return None
#' @export
#'
#' @examples
#' data(dsfeatfreqnegative)
#' # testCompareDsFeatFreq(dsfeatfreqnegative, 'Negative', dataset.colors[c(1:5, 7)])
#' testCompareDsFeatFreq(dsfeatfreqnegative, 'Negative', NULL)
testCompareDsFeatFreq<-function(df, condition, palette)
{
  df_melted <- meltDataFrame(df)
  plotQQ(df, condition)
  pressEnterToContinue(
    'Quantile-Quantile chart is drawn. Waiting Kruskal-Wallis Violin Chart... ')
  plotDsFreqDistributionViolin(df_melted, condition=condition, ds_palette=palette)
}

#' Test heatmap comparison visualization for positive datasets
#'
#' Test (demonstration) for plotting a heatmap from two dimensional P value
#' matrix for datasets with positive class.
#'
#' @param wb Excel workbook path
#'
#' @return None
#' @export
#'
#' @examples
#' # testComparePositiveDsPValueHeatmap(wb='PositiveDSMeanDifferencePValues.xlsx')
testComparePositiveDsPValueHeatmap<-function(wb='Datasets/PositiveDSMeanDifferencePValues.xlsx')
{
  df_multicompare <- loadPairwiseDsComparisonOfMeanRanks(wb)
  testCompareDsPValueHeatmap(df_multicompare)
}

#' Test heatmap comparison visualization for negative datasets
#'
#' Test (demonstration) for plotting a heatmap from two dimensional P value
#' matrix for datasets with negative class.
#'
#' @param wb Excel workbook path
#'
#' @return None
#' @export
#'
#' @examples
#' # testCompareNegativeDsPValueHeatmap(wb='NegativeDSMeanDifferencePValues.xlsx')
testCompareNegativeDsPValueHeatmap<-function(wb='Datasets/NegativeDSMeanDifferencePValues.xlsx')
{
  df_multicompare <- loadPairwiseDsComparisonOfMeanRanks(wb)
  testCompareDsPValueHeatmap(df_multicompare)
}

#' Test heatmap comparison visualization for positive datasets
#'
#' Test (demonstration) for plotting a heatmap from two dimensional P value
#' matrix for datasets with positive/negative class.
#'
#' @param df_multicompare two dimensional matrix around PValue
#'
#' @return None
#' @export
#'
#' @examples
#' data(dscompmeanrankspositive)
#' testCompareDsPValueHeatmap(dscompmeanrankspositive)
testCompareDsPValueHeatmap<-function(df_multicompare)
{
  mat_pairwise_ds_pvalues <- getPairwiseDsPValueMatrix(df_multicompare)
  plotPairwiseDsPValuesHeatMap(mat_pairwise_ds_pvalues)
}
