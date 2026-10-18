#' Pearson's dataset of Heights of fathers and their sons, that he used to describe regression.
#'
#' A classic dataset containing 1,078 measurements of a father's height 
#' and his adult son's height, collected by Karl Pearson.
#'
#' @format A data frame with 1078 observations on 2 variables:
#' \describe{
#'   \item{fheight}{Father's height in inches.}
#'   \item{sheight}{Son's height in inches.}
#' }
#' @source Pearson, K. and Lee, A. (1903). On the laws of inheritance in man. 
#'   \emph{Biometrika}, 2(4), 357-462.
"father.son"




#' Bodyweights of Female Mice, fed on high fat (hf) vs control (chow) diet.
#'
#' This dataset contains bodyweights of female mice that were fed a high fat diet (hf-12 female mice) and their control (chow-12 female mice), fed on a normal/chow diet.
#' This dataset was curated by Rafael Irizarry for his book 'Data Analysis for the Life Sciences'
#' and the corresponding Harvard course-PH525x. The dataset can be accessed (along with other important biological datasets) on the official github repository
#' of the Harvard course-PH525x; The repository is called 'genomicsclass' and the datasets can be found at 'https://github.com'.
#' @format A dataframe with 24 rows and 2 variables:
#' \describe{
#' 	\item{diet}{either high fat diet or chow diet the female mice were fed.}
#' 	\item{bodyweight}{bodyweights of female mice in grams.}
#'}
#' @source Irizarry, R.A., & Love, M.I. (2016). Data Analysis for the Life Sciences with R (1st ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315367002
#' @source https://github.com/genomicsclass/dagdata/tree/master/inst/extdata
"femaleMiceWeights"






#' Bodyweights of female mice fed on control (chow) diet; an extensive dataset collected in Jackson laboratory. 
#'
#' This dataset is a part of a list of datasets, curated by Rafael Irizarry, for his book 'Data Analysis for Life Sciences' and the corresponding course at HarvardX.
#'
#' The dataset contains bodyweights of female mice that were fed on control diet in the highly revered Jackson Laboratory.
#'
#'Rafael uses this dataset as an entire population of mice to calculate Population summary statistics(Population mean, Population SD,etc.),which is rarely witnessed in real life, as most statisticians and data analysts work with sample populations derived from the actual Population, instead of the entire population itself because it's almost almost infeasible to measure any given metric for the entire poulation. 
#'
#' @format A double numeric vector with 225 rows
#' @source Jackson laboratory
#' @source https://github.com/genomicsclass/dagdata/tree/master/inst/extdata
#' @source Irizarry, R.A., & Love, M.I. (2016). Data Analysis for the Life Sciences with R (1st ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315367002
"femaleControlsPopulation"
