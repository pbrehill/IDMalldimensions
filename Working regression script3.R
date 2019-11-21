# Include packages
# install.packages("dplyr")
# install.packages("ordinal")
library(ordinal)
source("importclean.R")
source("regressions.R")
# source("generalhelpers.R")
source("ICCscript.R")
library(dplyr)

# Import data
bigimport <- import.clean("fiji_newscored_cleaned_Oct19.csv")
fiji_jst_scores <- bigimport[[1]]
fiji_scored <- bigimport[[2]]

# Set dimension names
dim_names <- c("Food", "Water", "Shelter", "Health", "Education", "Energy", "Sanitation",
               "Relationships", "Clothing", "Violence", "Family Planning", "Environment",
               "Voice", "Time-use", "Work")

# Set dimensions to study (not 10 or 11)
main_scores <- c("score1", "score2", "score3", "score4",  "score5", "score6", "score7",  
                 "score8", "score9", "score12", "score13", "score14", "score15")

# Get ICCs
results_ICC <- ICC_fiji(smallimport, main_scores)
results_ICC$Dimension <- main_scores

# Set control variables
control_vars <- c("age", "disability3", "rural", "urban", "itaukei", "indian")

# Create a function to run all regressions
all_the_regression = function(score, data = fiji_jst_scores){
  
  reg1 <- try(regression(score, model='clmm',
                             model_name='clmm_sex', ivs=append(control_vars, "sex"), data = data))
  reg2 <- try(regression(score, model='clm', 
                             model_name='clm_hhh_sex', ivs=append(control_vars, "hhh_sex"), data = data))
  reg3 <- try(regression(score, model='clm', 
                         model_name='clm_sex', ivs=append(control_vars, "sex"), data = data))
  
  coef.df1 <- check_regression(reg1[[1]])
  coef.df2 <- check_regression(reg2[[1]])
  coef.df3 <- check_regression(reg3[[1]])
  
  df_list <- list(coef.df1, coef.df2, coef.df3)
  names(df_list) <- c("clmm_sex", "clm_hhh_sex", "clm_sex")
  # deviance <- try(anova(reg1[[2]], reg2[[2]], type=2))
  
  return(list(df_list))
  
}

dimensions_list <- list()
anovas_list <- list()
# dimensions_list <- list()

# WIP - List of scores to regress

all_themes <- append(main_scores, grep("score*[1-9]+\\.", colnames(fiji_scored), value = TRUE))

# for (i in 1:15) {
#   score_name <- paste0('score', i)
#   regression_results <- all_the_regression(score_name)
#   dimensions_list[[score_name]] <- regression_results[[1]]
#   anovas_list[[score_name]] <- regression_results[[2]]
# 
#   # Update progress bar
#   setTxtProgressBar(pb, i)
# }

pb2 <- txtProgressBar(min = 0, max = length(all_themes), style = 3)

# Run lower level regressions
for (i in 1:length(all_themes)) {
  score_name <- all_themes[i]
  regression_results <- all_the_regression(score_name)
  dimensions_list[[score_name]] <- regression_results[[1]]
  # anovas_list[[score_name]] <- regression_results[[2]]
  
  # Update progress bar
  setTxtProgressBar(pb2, i)
}

# Extract household head dimension estimates
# hhh_ests_point <- get_dim_estimates(dimensions_list, "point", 2 ,"hhh_sex2")
# hhh_ests_lower <- get_dim_estimates(dimensions_list, "lower", 2 ,"hhh_sex2")
# hhh_ests_upper <- get_dim_estimates(dimensions_list, "upper", 2 ,"hhh_sex2")
# hhh_ests_df <- data.frame(lower = hhh_ests_lower, point = hhh_ests_point, upper = hhh_ests_upper)
# row.names(hhh_ests_df) <- names(dimensions_list)
# 
# # Extract sex dimension estimates
# sex_ests_point <- get_dim_estimates(dimensions_list, "point", 1 ,"sex.L")
# sex_ests_lower <- get_dim_estimates(dimensions_list, "lower", 1 ,"sex.L")
# sex_ests_upper <- get_dim_estimates(dimensions_list, "upper", 1 ,"sex.L")
# sex_ests_df <- data.frame(lower = sex_ests_lower, point = sex_ests_point, upper = sex_ests_upper)
# row.names(sex_ests_df) <- names(dimensions_list)

# Get all sex coefficients - CLMM
coefficients_list_ind <- list()
for(i in 1:length(colnames(dimensions_list[[1]][[1]]))){
  coef_name <- colnames(dimensions_list[[1]][[1]])[i]
  try(coefficients_list_ind[[coef_name]] <- extract_coefficient(dimensions_list, modelno = 1, variable = coef_name))
}
# Combine into one df
sex_ests_df <- compile_all_coef(all_themes, coefficients_list_ind, dimensions_list, 1)

# Get all hhh coefficients
coefficients_list_hhh <- list()
for(i in 1:length(colnames(dimensions_list[[1]][[2]]))){
  coef_name <- colnames(dimensions_list[[1]][[2]])[i]
  try(coefficients_list_hhh[[coef_name]] <- extract_coefficient(dimensions_list, modelno = 2, variable = coef_name))
}
# Combine into one df
hhh_ests_df <- compile_all_coef(all_themes, coefficients_list_hhh, dimensions_list, 2)

# Get all sex coefficients - CLM
coefficients_list_ind_clm <- list()
for(i in 1:length(colnames(dimensions_list[[1]][[3]]))){
  coef_name <- colnames(dimensions_list[[1]][[3]])[i]
  try(coefficients_list_ind_clm[[coef_name]] <- extract_coefficient(dimensions_list, modelno = 3, variable = coef_name))
}
# Combine into one df
sex_ests_clm_df <- compile_all_coef(all_themes, coefficients_list_ind_clm, dimensions_list, 3)

# 
# # # Extract estimates for model comparison
# # goodness_fit <- anova_estimates(anovas_list)
# # goodness_fit <- mutate(goodness_fit, logLik_difference = `sex_model logLik` - `hhh_model logLik`)
# 
# Export CSVs
write.csv(hhh_ests_df, 'hhh_ests.csv')
write.csv(sex_ests_df, 'sex_ests.csv')
write.csv(sex_ests_clm_df, 'sex_ests_clm.csv')
write.csv(results_ICC, 'ICCs.csv')
# write.csv(goodness_fit, 'model_comparison.csv')
