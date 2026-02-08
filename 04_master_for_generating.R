# ==============================================================================
# Script to Generate All MA Reports
# ==============================================================================
# Load required libraries
if (!require("quarto", quietly = TRUE)) {
  install.packages("quarto")
  library(quarto)
}
if (!require("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

# Define name of the singular MA qmd
input_qmd <- "singular_MA.qmd"
path <- file.path("Output", "cis.rds")
input <- readRDS(path)
df_estimates <- input$df_estimates

# Create Reports directory if it doesn't exist
reports_dir <- file.path("Output", "Reports")
if (!dir.exists(reports_dir)) {
  dir.create(reports_dir, recursive = TRUE)
  cat("Created directory:", reports_dir, "\n")
}

# Get unique MA numbers with their info
ma_info <- df_estimates %>%
  select(no, identifier, sheet_name) %>%
  distinct() %>%
  arrange(no)

cat("Found", nrow(ma_info), "unique meta-analyses\n\n")

# ---- Generate Reports for Each MA ----
# Track progress
total_mas <- nrow(ma_info)
successful <- 0
failed <- 0
error_log <- list()

cat("Starting report generation...\n")
cat(rep("=", 70), "\n", sep = "")

for (i in 1:nrow(ma_info)) {
  ma_no <- ma_info$no[i]
  ma_id <- ma_info$identifier[i]
  sheet <- ma_info$sheet_name[i]
  
  cat(sprintf("\n[%d/%d] Processing MA #%d: %s\n", 
              i, total_mas, ma_no, ma_id))
  cat(sprintf("        Sheet: %s\n", sheet))
  
  # Without this gsub, it wouldn't run, we need to normalize the id
  safe_id <- gsub("[^A-Za-z0-9_-]", "_", ma_id)
  output_file <- file.path(reports_dir, sprintf("%s.html", safe_id))
  
  # Try to render the report
  tryCatch({
    quarto::quarto_render(
      input = input_qmd,
      execute_params = list(
        ma_no = ma_no,
        ma_title = ma_id
      ),
      output_file = basename(output_file),
      output_format = "html",
      quiet = FALSE,
      pandoc_args = c("--embed-resources", "--standalone")
    )
    
    # Move file to output directory
    if (file.exists(basename(output_file))) {
      file.rename(basename(output_file), output_file)
    }
    
    successful <- successful + 1
    cat(sprintf("        :) Successfully created: %s\n", output_file))
    
  }, error = function(e) {
    failed <- failed + 1
    error_msg <- as.character(e)
    error_log[[as.character(ma_no)]] <<- list(
      ma_no = ma_no,
      ma_id = ma_id,
      error = error_msg
    )
    cat(sprintf("        X ERROR: %s\n", error_msg))
  })
}

# ---- Summary ----
cat("\n", rep("=", 70), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("Total MAs processed: %d\n", total_mas))
cat(sprintf("Successful: %d\n", successful))
cat(sprintf("Failed: %d\n", failed))

if (failed > 0) {
  cat("\n", rep("-", 70), "\n", sep = "")
  cat("ERRORS:\n")
  cat(rep("-", 70), "\n", sep = "")
  for (err in error_log) {
    cat(sprintf("\nMA #%d (%s):\n", err$ma_no, err$ma_id))
    cat(sprintf("  %s\n", err$error))
  }
}

cat("\nAll reports saved in:", reports_dir, "\n")






