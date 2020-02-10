library(dplyr)

dim_names <- c("Food", "Water", "Shelter", "Health", "Education", "Energy", "Sanitation",
               "Relationships", "Clothing", "Violence", "Family Planning", "Environment",
               "Voice", "Time-use", "Work")

get_dim_estimates <- function(dimensions_list=dimensions_list, statistic='point', model, variable){
  
  # Set row index (which statistic we're getting)
  if (statistic == 'point'){
    selector <- 1
  } else if (statistic == 'lower') {
    selector <- 2
  } else if (statistic == 'upper') {
    selector <- 3
  } else if (is.numeric(statistic) && statistic >= 1 && statistic <= 3) {
    selector <- statistic
  } else {
    stop("Invalid statistic selector")
  }
  
  # Get estimates
  best_estimates <- c()
  for (i in 1:length(dimensions_list)){
    new_best <- try(dimensions_list[[i]][[model]][selector ,variable])
    if (length(new_best == 1) && is.numeric(new_best)) {
      best_estimates[i] <- new_best
    } else {
      best_estimates[i] <- NA
    }
  }
  # names(best_estimates) <- dim_names
  
  return(best_estimates)
  
}

# Paste problem
paste2 <- function(string) {
  if(is.na(string)) {
    return(NA)
  }
  else{
    return(paste0(string))
  }
}

anova_estimates <- function(anovaslist=anovas_list){
  df <- data.frame(matrix(nrow = length(anovaslist), ncol = 4))
  
  for (i in 1:length(anovaslist)){
    df[i, 1] <- as.numeric(try(anovaslist[[i]][1, "logLik"]))
    df[i, 2] <- as.numeric(try(anovaslist[[i]][2, "logLik"]))
    df[i, 3] <- as.numeric(try(anovaslist[[i]][1, "Pr(>Chisq)"]))
    df[i, 4] <- as.numeric(try(anovaslist[[i]][2, "Pr(>Chisq)"]))
  }
  
  colnames(df) <- c("sex_model logLik", "hhh_model logLik", "sex_model Pr(>Chisq)",
                    "hhh_model Pr(>Chisq)")
  return(df)
}

extract_coefficient <- function(modelno, variable, dimlist = dimensions_list){
  hhh_ests_point <- get_dim_estimates(dimlist, "point", modelno, variable)
  hhh_ests_lower <- get_dim_estimates(dimlist, "lower", modelno, variable)
  hhh_ests_upper <- get_dim_estimates(dimlist, "upper", modelno, variable)
  hhh_ests_df <- data.frame(point = hhh_ests_point, lower = hhh_ests_lower, upper = hhh_ests_upper)
  row.names(hhh_ests_df) <- names(dimlist)
  
  return(hhh_ests_df)
}

compile_all_coef <- function(scores, coefficients_list, dimlist, modelno){
  sex_all_coef_df <- data.frame(matrix(nrow = length(all_themes), ncol = length(coefficients_list)))
  row.names(sex_all_coef_df) <- all_themes
  colnames(sex_all_coef_df) <- colnames(dimensions_list[[1]][[modelno]])
  for (i in 1:length(coefficients_list)){
    coef_name <- colnames(dimensions_list[[1]][[modelno]])[i]
    sex_all_coef_df[coef_name] <- paste(round(coefficients_list[[coef_name]]$point, 2),
                                        ' (',
                                        round(coefficients_list[[coef_name]]$lower, 2),
                                        ' / ',
                                        round(coefficients_list[[coef_name]]$upper, 2), 
                                        ')')
  }
  
  return(sex_all_coef_df)
}

# coefficients_list_ind <- list()
# for(i in 1:length(colnames(dimensions_list[[1]][[1]]))){
#   coef_name <- colnames(dimensions_list[[1]][[1]])[i]
#   try(coefficients_list_ind[[coef_name]] <- extract_coefficient(dimensions_list, modelno = 1, variable = coef_name))
# }