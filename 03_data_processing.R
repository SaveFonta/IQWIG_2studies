# ----- IQWiG Meta-Analysis Data Processing with escalc -----


#Define output file name from data cleaning 
file_name <- "data_two_studies.rds"

#Ouput directory 
inp <- file.path("Output", file_name)



# # Load your merged data
data_two_studies <- readRDS(inp)


#load useful functions
source("00_utilities.R")

# # Process the data using escalc wrapper 
df_estimates <- process_escalc(data_two_studies)


#  View distribution of data reports
table(df_estimates$data_report, useNA = "always")


source("00_confMeta_parallelized.R") #I'd like to add this part to the libarry confMeta
estimates <- df_estimates
time <- system.time ({
  cis <- confMeta.full(estimates, include_bayesian= FALSE, generate_plot = FALSE)
})


#save df_estimates and cis 
results <- list(
  df_estimates = df_estimates,
  cis = cis 
)


#decide the name for the output file
name <- "cis.rds"


out <- file.path("Output", name)

saveRDS(results, file = out)


#load cis
#cis <- readRDS("cis.rds")


