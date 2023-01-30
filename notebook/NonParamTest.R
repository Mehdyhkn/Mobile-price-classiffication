library(arsenal)
library(dplyr)
# on garde variables radiomics + pcr

run_non_parametric_tests_DFS <- function(data){
  #' @param df The dataframe containing the data 
  tab1 <- tableby(delta_DFS ~ ., dplyr::select(data, -SOPHIA ),
                           numeric.test = "kwt", cat.test = "fe", simulate.p.value=TRUE)
  tab2 <- cbind(as.data.frame(summary(tab1)),
                         "Adjusted p-value" = as.data.frame(summary(padjust(tab1, method = "BH")))$`p value`)
  return(tab2)
}

run_non_parametric_tests_OSAR <- function(data){
  #' @param df The dataframe containing the data 
  tab1 <- tableby(delta_OSAR ~ ., dplyr::select(data, -SOPHIA ),
                  numeric.test = "kwt", cat.test = "fe", simulate.p.value=TRUE)
  tab2 <- cbind(as.data.frame(summary(tab1)),
                "Adjusted p-value" = as.data.frame(summary(padjust(tab1, method = "BH")))$`p value`)
  return(tab2)
}

run_descriptive_analysis <- function(data){
  #' @param df The dataframe containing the data 
  mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                                 numeric.test="kwt", cat.test="chisq",
                                 numeric.stats=c("N", "mean", "sd" ), # ,"q1q3"),
                                 cat.stats=c("countpct"),
                                 stats.labels=list(N='Count', mean='mean', SD="sd"))# , q1q3='Q1,Q3'))
  factors <- colnames(data)
  tab2_desc <- tableby(as.formula(paste("delta_DFS~", paste(factors, collapse="+"))),
                       control=mycontrols)
  tab2_desc <- as.data.frame(summary(tab2_desc)) 
  return(tab2_desc)
}

