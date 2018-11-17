#' # DsProfiling - Dataset Profiling
#' @author Gürol Canbek, <gurol44@gmail.com>  
#' Copyright (C) 2017-2018 Gürol CANBEK  
#' This file is licensed under  
#' 
#'   A p a c h e   L i c e n s e   2 . 0  
#' 
#' A permissive license whose main conditions require preservation of copyright  
#' and license notices. Contributors provide an express grant of patent rights.  
#' Licensed works, modifications, and larger works may be distributed under  
#' different terms and without source code.  
#'  
#' See the license file in <https://github.com/gurol/dsprofiling>  
#' @references <http://gurol.canbek.com>  
#' @keywords dataset, profiling, data quality, quanitative analysis, benchmark  
#' @title Dataset Profiling  
#' @version 1.0  
#' @description R functions for calculating some of the profiling criteria for  
#' the datasets
#' @note version history  
#' 1.0, 17 June 2017, The first version  
#' @date 17 June 2017  

#' libraries  
# None

# CSV datasets should be converted to columnar format
# dfPermVT2018 <- convertCsvToDataFrame(dfPermCsvVT2018, sep=',', filter=dfStandard)


#' ### replaceNotNaValues
#' Replace the NA or NaN values in a dataframe with given value  
#' **Parameters:**  
#' *df*: Input dataset  
#' *new_value*: Value to be replaced for NA or NaN values  
#' **Return:**  
#' Replaced data frame  
#' **Example Usage:** 
# dfPermAMD <- replaceNotNaValues(dfPermAMD, TRUE)
replaceNotNaValues<-function(df, new_value)
{
  df[!is.na(df)] <- new_value
  
  return (df)
}

#' ### profileDatasetDensity
#' Return the density profiling criterion of a dataset  
#' **Parameters:**  
#' *df*: Input dataset  
#' **Return:**  
#' Calculated density  
#' **Example Usage:** 
# densityAMD <- profileDatasetDensity(dfPermAMD)
profileDatasetDensity<-function(df)
{
  return (length(df[!is.na(df)])/(nrow(df)*ncol(df)))
}

#' ### profileDatasetSparsity
#' Return the sparsity profiling criterion of a dataset  
#' **Parameters:**  
#' *df*: Input dataset  
#' **Return:**  
#' Calculated sparsity  
#' **Example Usage:** 
# sparsityAMD <- profileDatasetSparsity(dfPermAMD)
profileDatasetSparsity<-function(df)
{
  return (1-profileDatasetDensity(df))
}