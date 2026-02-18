#############################################
##        04_master_for_generating         ##
#############################################




library(meta)
library(dplyr)
library(stringr)

#Define output file name from data cleaning 
file_name   <- "data_two_studies.rds"
inp         <- file.path("Output", file_name) ##Ouput directory 


# New directories 
REPORTS_DIR <- file.path("Output", "Reports")  #where the reports will go
TEMP_DIR    <- file.path("Output", "Temp_Render_Files") #temporary, after this should be empty
BATCH_DIR   <- file.path("Output", "Batches_Intermediate")  #containing the batches with FULL objects (heavy on memory)

if (!dir.exists(REPORTS_DIR)) dir.create(REPORTS_DIR, recursive = TRUE)
if (!dir.exists(TEMP_DIR)) dir.create(TEMP_DIR, recursive = TRUE)
if (!dir.exists(BATCH_DIR)) dir.create(BATCH_DIR, recursive = TRUE)



# # Load your merged data
data_two_studies <- readRDS(inp)

#load useful functions
source("00_utilities.R")
source("00_confMeta_parallelized.R") 

# Process Data
df_estimates <- process_escalc(data_two_studies, MH = TRUE)
df_estimates <- df_estimates %>% 
  mutate(effect.measure = ifelse(effect.measure == "OR (effekt)", "OR", effect.measure)) %>% 
  mutate(effect.measure = ifelse(effect.measure == "RR (effekt)", "RR", effect.measure))






########################################
# --- CALCULATION (With the plot!) --- #
########################################


unique_ids <- unique(df_estimates$no)
num_batches <- 20 #20 in arbitrary but seems to make the computer lag not too much
id_chunks <- split(unique_ids, cut(seq_along(unique_ids), breaks = num_batches, labels = FALSE))

message(sprintf("--- Processing %d batches of IDs ---", num_batches))

for (i in 1:num_batches) {
  
  batch_file <- file.path(BATCH_DIR, paste0("batch_", i, ".rds"))
  
  # If batch is already on disk, skip it... so we can stop the process
  if (file.exists(batch_file)) {
    message(sprintf("Batch %d already done. Skipping.", i))
    next
  }
  
  message(sprintf("Processing Batch %d/%d...", i, num_batches))
  
  # Filter Data
  batch_ids <- id_chunks[[i]]
  batch_data <- df_estimates %>% filter(no %in% batch_ids)
  
  # Run Calculation
  batch_results <- tryCatch({
    confMeta.full(batch_data, 
                  include_bayesian = FALSE, 
                  generate_plot = TRUE, 
                  MH = TRUE, 
                  parallel = TRUE) 
  }, error = function(e) {
    message("Error in batch ", i, ": ", e$message)
    return(NULL)
  })
  
  
  
  # Save
  if (!is.null(batch_results)) {
    saveRDS(batch_results, batch_file)
  }
  
  # Delete everything
  rm(batch_results, batch_data)
  gc() # FONDAMENTAL for really cleaning up the space
  message(sprintf("Batch %d saved", i))
}

message("Calculations Complete. :)")





####################
# --- RENDERING ---#
####################

# Get list of batch files inside the directory
batch_files <- list.files(BATCH_DIR, pattern = "batch_.*\\.rds", full.names = TRUE)

#order it 
batch_files <- batch_files[str_order(batch_files, numeric = TRUE)]


message("--- Starting Report Rendering ---")

SKIP <- TRUE  #set to TRUE to skip already rendered ones

for (b_idx in seq_along(batch_files)) {
  current_batch_results <- readRDS(batch_files[b_idx])
  current_batch_results <- check_lemma_conditions(current_batch_results, alpha = 0.05)
  
  
  # Loop through studies inside the batch
  for (j in seq_along(current_batch_results)) {

    res_obj <- current_batch_results[[j]]
    
    ma_id   <- res_obj$ma_id 
    safe_id <- gsub("[^A-Za-z0-9_-]", "_", ma_id)
    final_html <- file.path(REPORTS_DIR, paste0(safe_id, ".html"))
    
    # SKIP 
    if (SKIP && file.exists(final_html)) {
      next 
    }
    
    # Create temporary param for Quarto
    temp <- file.path(TEMP_DIR, paste0("data_", safe_id, ".rds"))  
    saveRDS(list(res_obj), temp) 
    
    try({
      quarto::quarto_render(
        input = "00_singular_MA.qmd",
        execute_params = list(result_path = temp),
        output_file = paste0(safe_id, ".html"),
        quiet = TRUE
      )
      
      if (file.exists(paste0(safe_id, ".html"))) {
        file.rename(paste0(safe_id, ".html"), final_html)
      }
    }, silent = TRUE)
    
    if (file.exists(temp)) unlink(temp)
    sprintf("Finished rendering batch %d", j)
    
  }
  
  # Unload the batch
  rm(current_batch_results)
  gc()
}

message(":)))) All Done!")
