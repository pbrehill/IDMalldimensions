library(tidyverse)
library(ordinal)
library(ICC)
library(VGAM)
library(stargazer)
source("importclean.R")
source("regressions.R")
# source("generalhelpers.R")
# source("ICCscript.R")

# Import data
fiji_jst_scores <- import.clean("fiji_newscored_cleaned_Oct19.csv")

# Get regression scores
score_vars <- colnames(fiji_jst_scores)[grepl('score', colnames(fiji_jst_scores))] %>%
  .[!grepl('_', .)]
score_vars

# Health regression
select_cols <- c('hhh_sex', 'sex', 'age', 'disability3', 'urban', 'rural', 'itaukei', 'indian', 'informal', score_vars)
fiji_short <- fiji_jst_scores[select_cols]

# Loop through regression

# Create all result list
dimensions <- list()

# Create error log
elog <- c()

# Create progress bar
pb <- txtProgressBar(min = 0, max = length(score_vars), style = 3)

for (i in 1:length(score_vars)) {
  
  tryCatch({
  
    # Create one result list
    result <- list()
    
    # Test sex
    ## Get regression formula
    form_string <- paste0(score_vars[i], '~ sex + age + disability3 + itaukei + indian + rural')
    form <- eval(parse(text=form_string))
    
    ## Get CLM for nominal test (can't be contained in function due to bug)
    clm_test <- clm(form, data=fiji_short, Hess=T, nAGQ=10)
    ppo_result <- ppo_clm(form, fiji_short)
    result[["sex"]] <- sex_coefficient(ppo_result)
    
    # Test hhh_sex
    ## Get regression formula
    form_string <- paste0(score_vars[i], '~ hhh_sex + age + disability3 + itaukei + indian + rural')
    form <- eval(parse(text=form_string))
    
    ## Get CLM for nominal test (can't be contained in function due to bug)
    clm_test <- clm(form, data=fiji_short, Hess=T, nAGQ=10)
    ppo_result <- ppo_clm(form, fiji_short)
    result[["hhh_sex"]] <- sex_coefficient(ppo_result)
    
    # Save to results
    dimensions[[score_vars[i]]] <- result
  
  }, error = function (e) {
    elog <<- append(elog, paste0("Error on DV -", score_vars[i], '-: Error: ', conditionMessage(e)))
    print(paste0("Error on DV -", score_vars[i], '-: Error: ', conditionMessage(e)))
  })
  
  # Update PB
  setTxtProgressBar(pb, i)
}

# Write error log
readr::write_lines(elog, paste0('Errors/errors-', Sys.time(), '.txt'))

# Join tables

# Iterate through results
# pb2 <- txtProgressBar(min = 0, max = length(dimensions), style = 3)

for (i in 1:length(dimensions)) {
  
  # Pad dataframes
  while (nrow(dimensions[[i]][["sex"]]) < 3) {
    dimensions[[i]][["sex"]][nrow(dimensions[[i]][["sex"]])+1,] <- ""
  }
  
  while (nrow(dimensions[[i]][["hhh_sex"]]) < 3) {
    dimensions[[i]][["hhh_sex"]][nrow(dimensions[[i]][["hhh_sex"]])+1,] <- ""
  }
  
  # Join dfs
  dimensions[[i]][["joint"]] <- bind_cols(dimensions[[i]][["sex"]], dimensions[[i]][["hhh_sex"]])
  colnames(dimensions[[i]][["joint"]]) <- c("Sex_coefficient", "Sex_OR", "HHH_sex_coefficient", "HHH_sex_OR")
  
  # Update PB
  # setTxtProgressBar(pb2, i)
}

dim_list <- list()
non_dim_list <- list()

for (i in 1:length(dimensions)) {
  
  # Find out if a score is a dimension score
  if (!grepl('\\.', names(dimensions)[i])) {
  # These are dimension scores
    dim_list[[names(dimensions)[i]]] <- dimensions[[i]][["joint"]]
  } else {
    non_dim_list[[names(dimensions)[i]]] <- dimensions[[i]][["joint"]]
  }
}


stargazer(dim_list, title=names(dim_list), type="html", out="stargazertest1.html", 
          summary=rep(F,length(dim_list)))

stargazer(non_dim_list, title=names(non_dim_list), type="html", out="stargazertest2.html", 
          summary=rep(F,length(non_dim_list)))

# ICC

## Get plain dim names
just_dim_names <- colnames(fiji_jst_scores)[grepl('numeric', 
                                                  colnames(fiji_jst_scores))]

# BUG rounding from factor to numeric and back
ICC_df <- as.data.frame(just_dim_names)
for (i in 1:length(just_dim_names)) {
  ICCresults <- ICCest(x = fiji_jst_scores[,'HHID_unique'], 
                       y = unlist(fiji_jst_scores[just_dim_names[i]]))
  ICC_df[i, 2] <- ICCresults$ICC
  ICC_df[i, 3] <- ICCresults$LowerCI
  ICC_df[i, 4] <- ICCresults$UpperCI
}

colnames(ICC_df) <- c('Dimension', 'ICCest', 'LowerCI', 'UpperCI')
stargazer(ICC_df, type = "html", out = "ICC.html")

