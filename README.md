## DsFeatFreqComp – Dataset Feature-Frequency Comparison R Package: A Research Compendium of
# Gaining New Insight in Machine-Learning Datasets via Multiple Binary-Feature Frequency Ranks with a Mobile Benign/Malware Apps Example

[![Last-changedate](https://img.shields.io/badge/last%20change-2020--09--02-brightgreen.svg)](https://github.com/gurol/dsfeatfreqcomp) [![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)  [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--9337--097X-green.svg)](https://orcid.org/0000-0002-9337-097X)

This platform is a research compendium of my academic publication below.

> [Gürol Canbek](http:gurol.canbek.com/Publications). *Gaining New Insight in Machine-Learning Datasets via Multiple Binary-Feature Frequency Ranks with a Mobile Benign/Malware Apps Example*, [Journal of Information Security and Applications (JISAS)](https://www.journals.elsevier.com/journal-of-information-security-and-applications), 2020 (*In Review*).

The DsFeatFreqComp package provides two categories of important functions: dataset manipulation and visualization.

## Related visualization functions:
- plotDsFreqDistributionViolin
- plotQQ
- plotPairwiseDsPValuesHeatMap

## Dataset manipulation functions:
The dataset manipulation functions are
- loadDsFeatFreqsFromCsv2
- meltDataFrame
- loadPairwiseDsComparisonOfMeanRanks
- getPairwiseDsPValueMatrix

## Abstract
Researchers compare their Machine Learning (ML) classification performances with other studies without examining and comparing the datasets they used in training, validating, and testing. One of the reasons is that there are not many convenient methods to give initial insights about datasets besides the descriptive statistics applied to individual continuous or quantitative features. After demonstrating initial manual analysis techniques, this study proposes a novel adaptation of the Kruskal-Wallis statistical test to compare a group of datasets over multiple prominent binary features that are very common in today’s datasets. As an illustrative example, the new method was tested on six benign/malign mobile application datasets over the frequencies of prominent binary-features to explore the dissimilarity of the datasets per class. The feature vector consists of over a hundred “application permission requests” that are binary flags for Android platforms’ primary access control to provide privacy and secure data/information in mobile devices. Permissions are also the first leading transparent features for ML-based malware classification. The proposed data analytical methodology can be applied in any domain through their prominent features of interest. The results, which are also visualized in new ways, have shown that the proposed method gives the dissimilarity degree among the datasets. Specifically, the conducted test shows that the frequencies in the aggregated dataset and some of the datasets are not substantially different from each other even they are in close agreement in positive-class datasets. It is expected that the proposed domain-independent method brings useful initial insight to researchers on comparing different datasets.

## Keywords
Machine learning; binary classification; dataset comparison; dataset profiling; feature engineering; quantitative analysis; data quality; Android; malware detection

## Package Installation
### From this GitHub repository
1. Load the devtools package by `library(devtools)`. If the package is not installed already install the package by `install.packages("devtools")`
2. Install the package from the repository by `install_github("gurol/DsFeatFreqComp")`.
### From local package archive file downloaded on a computer
Go the the file's directory and run the following command in terminal
`install.packages('DsFeatFreqComp_1.0.0.tar.gz', repos = NULL, type = 'source')`

## The extra information about datasets
[DsFeatFreqDistFit - Dataset Feature-Frequency Distributions Fitting](https://github.com/gurol/DsFeatFreqDistFit)
