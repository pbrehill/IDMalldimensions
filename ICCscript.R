library(ICC)
source("importclean.R")

smallimport <- import.clean("fiji_newscored_cleaned_Oct19.csv")[[2]]

ICC_fiji <- function(df, scores){
  fiji_icc_df <- df
  ls <- list()
  for (i in 1:length(scores)){
    
    results_vec <- c()
    fiji_icc_df$HHID_unique <- as.factor(fiji_icc_df$HHID_unique)
    scoreno <- scores[i]
    ICC_formula <- paste0("ICCest(HHID_unique, ",scoreno,", data = fiji_icc_df, alpha = 0.05)")
    results <- try(eval(parse(text=ICC_formula)))
    if (class(results) == "try-error"){
      next
    }
    # results_vec[1] <- results[["ICC"]]
    # results_vec[2] <- results[["LowerCI"]]
    # results_vec[3] <- results[["UpperCI"]]
    # names(results_vec) <- c("point_est", "2.5_est", "97.5_est")
    results[[length(results) + 1]] <- scoreno
    ls[[i]] <- results
  }
  
  # Organise results into a dataframe
  ICC_df <- data.frame(matrix(nrow=length(ls), ncol=4))
  for (i in 1:length(ls)){
    if (class(ls[[i]]) == "list") {
      ICC_df[i, 1] <- ls[[i]][[8]]
      ICC_df[i, 2] <- ls[[i]][["ICC"]]
      ICC_df[i, 3] <- ls[[i]][["LowerCI"]]
      ICC_df[i, 4] <- ls[[i]][["UpperCI"]]
    }
  }
  colnames(ICC_df) <- c("Dimension", "ICC est", "LowerCI", "UpperCI")
  
  return(ICC_df)
}

fiji_jst_scores %>%
  filter(score12 == 0) %>%
  #group_by(sex, score12) %>%
  summary()
  