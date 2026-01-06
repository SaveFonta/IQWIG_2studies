################################################################################
# Script to Render Individual Meta-Analysis Reports
# 
# This script loops through all meta-analyses and generates individual
# HTML reports organized by sheet name
################################################################################

library(quarto)
library(dplyr)

# ---- Load Data ----
cat("Loading data...\n")
input <- readRDS("cis.rds")
df_estimates <- input$df_estimates

# ---- Get Unique MAs with Sheet Names ----
# Create a lookup table: ma_no -> sheet_name
ma_info <- df_estimates[1:10,] %>%
  select(no, identifier, sheet_name) %>%
  distinct() %>%
  arrange(no)

cat("Found", nrow(ma_info), "unique meta-analyses\n\n")

# ---- Create Output Folder Structure ----
base_output_dir <- "Output"

# Create main Output directory
if (!dir.exists(base_output_dir)) {
  dir.create(base_output_dir)
  cat("Created directory:", base_output_dir, "\n")
}

# Create subdirectories for each sheet
sheet_names <- c("Dossiers", "A_NB", "D_NB", "N_NB", "S_NB", "EVB")

for (sheet in sheet_names) {
  sheet_dir <- file.path(base_output_dir, sheet)
  if (!dir.exists(sheet_dir)) {
    dir.create(sheet_dir, recursive = TRUE)
    cat("Created directory:", sheet_dir, "\n")
  }
}

cat("\n")

# ---- Render Each MA Report ----
cat("Starting to render reports...\n\n")

# Path to the Quarto template
qmd_file <- "Singular_MA.qmd"

# Check if template exists
if (!file.exists(qmd_file)) {
  stop("Quarto template '", qmd_file, "' not found!")
}

# Track progress and errors
n_total <- nrow(ma_info)
n_success <- 0
n_errors <- 0
errors_list <- list()

# Progress bar setup
pb <- txtProgressBar(min = 0, max = n_total, style = 3, width = 50)

# Loop through each MA
for (i in seq_len(n_total)) {
  
  ma_no <- ma_info$no[i]
  ma_id <- ma_info$id[i]
  sheet <- ma_info$sheet_name[i]
  
  # Define output file path
  output_file <- file.path(base_output_dir, sheet, paste0(ma_id, ".html"))
  
  # Try to render
  tryCatch({
    
    # Render with parameters
    quarto::quarto_render(
      input = qmd_file,
      execute_params = list(ma_no = ma_no),
      output_file = basename(output_file),
      output_format = "html",
      quiet = TRUE
    )
    
    # Move file to correct location if needed
    # (quarto_render with output_file uses basename, so file is in current dir)
    temp_output <- basename(output_file)
    if (file.exists(temp_output)) {
      file.rename(temp_output, output_file)
    }
    
    n_success <- n_success + 1
    
  }, error = function(e) {
    n_errors <- n_errors + 1
    errors_list[[length(errors_list) + 1]] <<- list(
      ma_no = ma_no,
      sheet = sheet,
      error = e$message
    )
    
    cat("\nError rendering MA", ma_no, "from sheet", sheet, ":", e$message, "\n")
  })
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

close(pb)

# ---- Summary ----
cat("\n\n")
cat("================================================================================\n")
cat("RENDERING COMPLETE\n")
cat("================================================================================\n")
cat("Total MAs:", n_total, "\n")
cat("Successfully rendered:", n_success, "\n")
cat("Errors:", n_errors, "\n")
cat("Output directory:", normalizePath(base_output_dir), "\n")

if (n_errors > 0) {
  cat("\n")
  cat("--------------------------------------------------------------------------------\n")
  cat("ERRORS SUMMARY:\n")
  cat("--------------------------------------------------------------------------------\n")
  for (err in errors_list) {
    cat(sprintf("MA %s (Sheet: %s): %s\n", err$ma_no, err$sheet, err$error))
  }
}

cat("\n")
cat("Reports are organized by sheet name in the Output folder.\n")
cat("================================================================================\n")