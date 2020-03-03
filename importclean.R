source("generalhelpers.R")
library(dplyr)


import.clean <- function(csv)
{
  # Import data
  fiji_scored <- read.csv(csv)
  
  fiji_jst_scores <- fiji_scored
    # dplyr::select(sex, age.categories, area, EA, sector, ethnicity, disability3, relationship,
    #                score1, score2, score3,
    #                score4, score5, score6, score7, score8, score9, score12, score13, score14, score15)

  for (i in 1:ncol(fiji_jst_scores)){
    if (grepl('score', colnames(fiji_jst_scores)[i])) {
      fiji_jst_scores[i] <- fiji_jst_scores[i] %>%
        round() %>%
        sapply(paste0) %>%
        factor(ordered = TRUE, exclude = c(NA, "NA"))
    }
  }
  
  # fiji_jst_scores <- fiji_scored %>%
  #   select(HHID_unique, age) %>%
  #   bind_cols(fiji_jst_scores)
  
  # Set household head
  fiji_jst_scores <- fiji_scored %>%
    filter(relationship == 1) %>%
    select(sex, HHID_unique) %>%
    dplyr::rename(hhh_sex = sex) %>%
    right_join(fiji_jst_scores, by = "HHID_unique")
  
  # Set ethnicity dummy variables
  fiji_jst_scores <- fiji_scored %>%
    mutate(itaukei = ethnicity == 1, indian = ethnicity == 2, other.ethnicity = ethnicity > 2) %>%
    select(itaukei, indian, other.ethnicity) %>%
    bind_cols(fiji_jst_scores)
  
  # Set sector dummy variables
  fiji_jst_scores <- fiji_scored %>%
    mutate(rural = sector == 1, urban = sector == 2, informal = sector == 3) %>%
    select(rural, urban, informal) %>%
    bind_cols(fiji_jst_scores)
  
  # Convert factors
  fiji_jst_scores$hhh_sex <- as.factor(fiji_jst_scores$hhh_sex)
  fiji_jst_scores$HHID_unique <- as.factor(fiji_jst_scores$HHID_unique)
  
  # Add unrounded scores
  fiji_jst_scores <- fiji_scored %>%
    select(score1, score2, score3,score4, score5, score6, score7, score8, score9, 
           score12, score13, score14, score15) %>%
    rename_all(.funs = funs(paste0(., '_numeric'))) %>%
    bind_cols(fiji_jst_scores)
  
  return(fiji_jst_scores)
}

fiji_jst_scores <- import.clean("fiji_newscored_cleaned_Oct19.csv")

