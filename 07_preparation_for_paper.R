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

#clean both the MH df and the IV df (noMH)
df_estimates <- df_estimates %>%
  mutate(effect.measure = ifelse(effect.measure == "OR (effekt)", "OR", effect.measure)) %>%
  mutate(effect.measure = ifelse(effect.measure == "RR (effekt)", "RR", effect.measure))

df_estimates_noMH <- df_estimates_noMH %>%
  mutate(effect.measure = ifelse(effect.measure == "OR (effekt)", "OR", effect.measure)) %>%
  mutate(effect.measure = ifelse(effect.measure == "RR (effekt)", "RR", effect.measure))

table(df_estimates$effect.measure) / 2

#PEFECT




# -------------------------------------------------------
# Now we can process usign the param 'methods_to_exclude' to decide which methods to exclude.
# If you don't want to exclude anything, just set 'methods_to_exclude = NULL'

#Process both, the only difference is that for the MAs that have 2x2 table, we use MH instead of IV, but
# the name of the method is always called Fixed-effect
time <- system.time({
  cis_iv <- confMeta.full(df_estimates_noMH,
                          include_bayesian    = FALSE,
                          generate_plot       = FALSE,
                          MH                  = FALSE,
                          parallel            = TRUE,
                          methods_to_exclude  = c("Henmi & Copas"))

  cis_mh <- confMeta.full(df_estimates,
                          include_bayesian    = FALSE,
                          generate_plot       = FALSE,
                          MH                  = TRUE,
                          parallel            = TRUE,
                          methods_to_exclude  = c("Henmi & Copas"))
})



#-----------------------------------------------------
#Now we need to add the MH to the methods

#find mas that used MH (has a non null 2x2)
mh_mas <- names(Filter(function(x) !is.null(x$table_2x2), cis_mh))
length(mh_mas) #number of MA using the MH




# -------------------------------------------------------
#Create the 3 complete df that can be used for the plots and summaries in the paper

# 1st DATAFRAME
# Contains all stats for every Method x MA combination
# so it has one row for each combination of meta analysis and method 
# "Fixed effect" from cis_iv  -> "Fixed-effect (IV)"
# "Fixed effect" from cis_mh  -> "Fixed-effect (MH)" for ALL MAs,
#   meaning: MH where available, IV fallback otherwise.

df_mas_iv <- map_dfr(cis_iv, function(x) {
  x$ci %>%
    as.data.frame() %>%
    mutate(
        method        = ifelse(method == "Fixed-effect", "Fixed-effect (IV)", method), #change the name FE to IV
      MA            = x$ma_id,
      measure       = x$measure,
      Q             = x$heterogeneity$Q,
      I2            = x$heterogeneity$I2,
      Tau2          = x$heterogeneity$Tau^2,
      data_skewness = x$data_skewness
    )
})

df_mas_mh_fe <- map_dfr(cis_mh, function(x) {
  x$ci %>%
    as.data.frame() %>%
    filter(method == "Fixed-effect") %>%  #just extract the FE
    mutate(
      method        = "Fixed-effect (MH)", #change it to MH
      MA            = x$ma_id,
      measure       = x$measure,
      Q             = x$heterogeneity$Q,
      I2            = x$heterogeneity$I2,
      Tau2          = x$heterogeneity$Tau^2,
      data_skewness = x$data_skewness
    )
})

df_mas <- bind_rows(df_mas_iv, df_mas_mh_fe) %>%
  rename(p_val = p_0) %>%
  arrange(MA, method)

# --> this mean that for EVERY MA we won't have Fixed-effect, but both Fixed-effect (IV) and Fixed-effect (MH)
# in the MAs where the 2x2 not present, then MH == IV


# 2ND
# Contains 1 row per MA with study-level info of the two individual studies (SEs, estimates, ....)
df_inputs <- map_dfr(cis_iv, function(x) { #uses cis_iv, but it is the same
  est <- x$inputs$estimate
  se  <- x$inputs$SE
  low <- x$inputs$lower
  upp <- x$inputs$upper
  
  data.frame(
    MA                  = x$ma_id,
    est_1               = est[1],
    est_2               = est[2],
    SE_1                = se[1],
    SE_2                = se[2],
    low_1               = low[1],
    low_2               = low[2],
    upp_1               = upp[1],
    upp_2               = upp[2],
    min_study_lower     = min(low),
    max_study_upper     = max(upp),
    SE_ratio            = max(se) / min(se),
    opposing_directions = (est[1] * est[2]) < 0,
    used_MH             = x$ma_id %in% mh_mas #add a flag
  )
})




#useful to compute the 3rd
df_fe_mh <- df_mas %>%
  filter(method == "Fixed-effect (MH)") %>%
  select(MA, width_fe = width, p_fe = p_val, est_fe = estimate)

df_fe_iv <- df_mas %>%
  filter(method == "Fixed-effect (IV)") %>%
  select(MA, width_fe_iv = width, p_fe_iv = p_val, est_fe_iv = estimate)


# 3rd 
#compares all methods vs FE 
df_comparisons <- df_mas %>%
  filter(!method %in% c("Fixed-effect (MH)", "Fixed-effect (IV)")) %>%
  left_join(df_fe_mh, by = "MA") %>%
  left_join(df_fe_iv, by = "MA") %>%
  mutate(
    # vs Fixed-effect (MH) (so keep the same logic)
    rel_width_increase = (width - width_fe) / width_fe,
    p_diff             = p_val - p_fe,
    shrinkage_ratio    = ifelse(abs(est_fe) < 1e-6, NA, estimate / est_fe),
    sign_flip          = sign(estimate) != sign(est_fe),
    
    # vs Fixed-effect (IV)
    rel_width_increase_iv = (width - width_fe_iv) / width_fe_iv,
    p_diff_iv             = p_val - p_fe_iv,
    shrinkage_ratio_iv    = ifelse(abs(est_fe_iv) < 1e-6, NA, estimate / est_fe_iv),
    sign_flip_iv          = sign(estimate) != sign(est_fe_iv)
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
    cis_iv         = cis_iv,
    cis_mh         = cis_mh,
    df_estimates = df_estimates
  ), 
  file = "Output/paper_analysis_data.rds"
)

# Save the dfs as csv
write.csv(df_mas, "Output/df_mas.csv", row.names = FALSE)
write.csv(df_inputs, "Output/df_inputs.csv", row.names = FALSE)
write.csv(df_comparisons, "Output/df_comparisons.csv", row.names = FALSE)
write.csv(df_estimates, "Output/df_estimates.csv", row.names = FALSE)



