#############################################
##           07_preparation_for_paper      ##
#############################################



library(dplyr)
library(meta)
library(purrr)


# ----------------------------------------------------
#Run the exact initial code used in 03_data_processing

file_name <- "data_two_studies.rds"
inp <- file.path("Output", file_name) 
data_two_studies <- readRDS(inp)
source("00_utilities.R")
source("00_confMeta_parallelized.R") 
df_estimates <- process_escalc(data_two_studies, MH = TRUE)
df_estimates_noMH <- process_escalc(data_two_studies, MH = FALSE)
table(df_estimates$data_report, useNA = "always")
table(df_estimates$effect.measure) /2
df_estimates <- df_estimates %>% 
  mutate (effect.measure = ifelse(effect.measure == "OR (effekt)", "OR", effect.measure)) %>% 
  mutate (effect.measure = ifelse(effect.measure == "RR (effekt)", "RR", effect.measure) )
table(df_estimates$effect.measure) / 2
#PEFECT






# -------------------------------------------------------
# Now we can process usign the param 'methods_to_exclude' to decide which methods to exclude.
# If you don't want to exclude anything, just set 'methods_to_exclude = NULL'

estimates <- df_estimates
time <- system.time ({
  cis <- confMeta.full(estimates, include_bayesian= FALSE, generate_plot = FALSE, 
                       MH = TRUE, parallel = TRUE, methods_to_exclude = c("Henmi & Copas"))
})



# -------------------------------------------------------
#Create the 3 complete df that can be used for the plots and summaries in the paper

# 1st DATAFRAME
# Contains all stats for every Method x MA combination
# so it has one row for each combination of meta analysis and method 

df_mas <- map_dfr(cis, function(x) {
  x$ci %>%
    as.data.frame() %>%
    mutate(
      MA = x$ma_id,
      measure = x$measure,
      # Add heterogeneity (repeated for all methods in this MA)
      Q = x$heterogeneity$Q,
      I2 = x$heterogeneity$I2,
      Tau2 = x$heterogeneity$Tau^2,
      
      data_skewness = x$data_skewness
    )
}) %>%
  rename(p_val = p_0) 

# 2ND
# Contains 1 row per MA with study-level info of the two individual studies (SEs, estimates, ....)

df_inputs <- map_dfr(cis, function(x) {
  est <- x$inputs$estimate
  se  <- x$inputs$SE
  low <- x$inputs$lower
  upp <- x$inputs$upper
  
  data.frame(
    MA = x$ma_id,
    est_1 = est[1], 
    est_2 = est[2],
    
    SE_1 = se[1],   
    SE_2 = se[2],
    
    
    low_1 = low[1],
    low_2 = low[2],
    upp_1 = upp[1],
    upp_2 = upp[2],
    min_study_lower = min(low),
    max_study_upper = max(upp),
    
    SE_ratio = max(se) / min(se),
    opposing_directions = (est[1] * est[2]) < 0
  )
})



#This is useful to build the 3rd one 
df_fe <- df_mas %>%
  filter(method == "Fixed effect") %>%
  select(MA, width_fe = width, p_fe = p_val, est_fe = estimate)


# 3rd 
#compares all methods vs FE 
df_comparisons <- df_mas %>%
  filter(method != "Fixed effect") %>%
  left_join(df_fe, by = "MA") %>%
  mutate(
    rel_width_increase = (width - width_fe) / width_fe,
    p_diff = p_val - p_fe,
    shrinkage_ratio = ifelse(abs(est_fe) < 1e-6, NA, estimate / est_fe),
    sign_flip = sign(estimate) != sign(est_fe)
  )



# NOTE: 
# This script could replace the 03_data_processing.R. However, doing so would mean that I would need to adapt
# the QMD reports to work with any choice of methods_to_exclude. The reports were infact written assuming
# methods_to_exclude = c("Random effects", "Henmi & Copas") and they will probably break if we feed them with 
# cis computed using different choices of methods_to_exclude. 

# I honestly don't think it is worth the effort, so I decided to create this new file so that the old analysis
# can still run without any problems. 




# ----------------------------------------
# SAVE  results

# save as RDS 
saveRDS( 
  list( 
    df_mas = df_mas,
    df_inputs = df_inputs, 
    df_comparisons = df_comparisons,
    cis = cis,
    df_estimates = df_estimates
  ), 
  file = "Output/paper_analysis_data.rds"
)

# Save the dfs as csv
write.csv(df_mas, "Output/df_mas.csv", row.names = FALSE)
write.csv(df_inputs, "Output/df_inputs.csv", row.names = FALSE)
write.csv(df_comparisons, "Output/df_comparisons.csv", row.names = FALSE)
write.csv(df_estimates, "Output/df_estimates.csv", row.names = FALSE)

