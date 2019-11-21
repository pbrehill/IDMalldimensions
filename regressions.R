library(tidyverse)

regression <- function(score, model, model_name, ivs, data=fiji_jst_scores){
  reg_ind_vars <- ivs
  if (model == "clmm") {
    reg_ind_vars <- append(reg_ind_vars, "(1|HHID_unique)")
  }
  fiji_reg_data <- data
  # # Remove incomplete cases to allow comparison of models
  # complete_cases_text <- paste0(deparse(substitute(data)), "[complete.cases(fiji_jst_scores[c('sex', 'hhh_sex')]), ]")
  # fiji_reg_data <- eval(parse(text = complete_cases_text))
  
  #Do an adjusted cumulative link mixed model (ordinal logistic regression mixed model). This is evaluating a weird string.
  expression_text <- paste0(model, "(", score, " ~", paste(reg_ind_vars, collapse = "+"), ", data=fiji_reg_data, Hess=T, nAGQ=10)")
  model <- eval(parse(text=expression_text))
  #get confidence intervals
  ci <- exp(confint(model))
  #exponentiate to get in terms of OR
  dfraw2 <- model[["coefficients"]][-1:-3] %>%
    exp() %>%
    as.data.frame()
  if (i == 15){
    ci
  }
  dfraw2$lower <- ci[rownames(dfraw2),1]
  dfraw2$upper <- ci[rownames(dfraw2),2]
  colnames(dfraw2) <- c(paste0(score, "mixed_sex_point_est"), paste0(score, "mixed_sex_2.5_est"), paste0(score, "mixed_sex_97.5_est"))
  coef.df <- as.data.frame(t(dfraw2))
  
  # Check random effects are normally distributed
  if (model == "clmm"){
    try(qqnorm(ranef(model)))
  }
  
  return(list(coef.df, model))
}

check_regression <- function(df){
  if (class(df) == "try-error"){
    return(as.data.frame(matrix(ncol=7, nrow=3)))
  } else {
    return(df)
  }
}
# 
# control_vars <- c("age", "disability3", "sector", "itaukei", "indian")
# test <- try(regression(score="score15", model='clm', model_name='clmm_sex', ivs=append(control_vars, "sex")))
# 
