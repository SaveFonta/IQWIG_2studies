# ----- IQWiG Meta-Analysis Data Processing with escalc -----
path <- "C:/Users/Menelao/Desktop/Held/confMeta/confMeta"

# load package
devtools::load_all(path)
#########################################
library(meta)


#Define output file name from data cleaning 
file_name <- "data_two_studies.rds"

#Ouput directory 
inp <- file.path("Output", file_name)



# # Load your merged data
data_two_studies <- readRDS(inp)


#load useful functions
source("00_utilities.R")

# # Process the data using escalc wrapper 
df_estimates <- process_escalc(data_two_studies, MH = TRUE)
df_estimates_noMH <- process_escalc(data_two_studies, MH = FALSE)


#  View distribution of data reports
table(df_estimates$data_report, useNA = "always")


#Now view distribution of effect measure
table(df_estimates$effect.measure) /2

# as you can see, there are columns with OR (effekt) and RR(effekt) --> let's merge them with OR and RR
df_estimates <- df_estimates %>% 
  mutate (effect.measure = ifelse(effect.measure == "OR (effekt)", "OR", effect.measure)) %>% 
  mutate (effect.measure = ifelse(effect.measure == "RR (effekt)", "RR", effect.measure) )

#Now view distribution of effect measure
table(df_estimates$effect.measure) / 2
#PEFECT

source("00_confMeta_parallelized.R") #I'd like to add this part to the libarry confMeta
estimates <- df_estimates
time <- system.time ({
  cis <- confMeta.full(estimates, include_bayesian= FALSE, generate_plot = FALSE, MH = TRUE, parallel = TRUE)
})


#add the lemma checking
alpha <- 0.05
cis <- check_lemma_conditions(cis, alpha = alpha)



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






