library(tidyverse)

ppo_clm <- function (formula, data) {
  
  # Get clm to test POA
  nomtest <- nominal_test(clm_test)
  not_validated <- na.omit(rownames(nomtest)[nomtest$`Pr(>Chi)` <= 0.05])
  
  
  # Run VGLM relaxing for IVs where POA is not met
  if (length(not_validated) == 0) {
    ppo_model <- vglm(formula, data = data, family=cumulative(parallel=TRUE, reverse = TRUE), na.action = na.omit)
  } else {
    not_val_formula <- paste('FALSE ~', paste0(not_validated, collapse = ' + '))
    not_val_formula <- eval(parse(text = not_val_formula))
    ppo_model <- vglm(formula, data = data, family=cumulative(parallel= not_val_formula, reverse = TRUE), na.action = na.omit)
  }
  
  # Return the model
  return(ppo_model)
}

sex_coefficient <- function(model) {
  return (summary(model)@coef3 %>%
            as.data.frame() %>%
            rownames_to_column('coef') %>%
            filter(grepl('sex', coef)) %>%
            select(coef, Estimate, `Pr(>|z|)`) %>%
            mutate(OR = exp(Estimate), stars = ifelse(`Pr(>|z|)` <= 0.05, "*", ""), 
                   `Sex OR` = paste0(format(round(OR, 3), nsmall = 3), 
                                     stars, 
                                     ' (p = ', 
                                     format(round(`Pr(>|z|)`, 3), nsmall = 3), 
                                     ') ')) %>%
            select(coef, `Sex OR`)
  )
}
