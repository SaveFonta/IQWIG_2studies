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

# ==============================================================================
# Setup
# ==============================================================================

# Define paths
input_qmd <- "singular_ma.qmd"  # Nome del tuo file Quarto
output_dir <- "Output"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# ==============================================================================
# Load Data and Get MA Numbers
# ==============================================================================

cat("Loading data from cis.rds...\n")
input <- readRDS("cis.rds")
df_estimates <- input$df_estimates

# Get unique MA numbers with their info
ma_info <- df_estimates %>%
  select(no, identifier, sheet_name) %>%
  distinct() %>%
  arrange(no)

cat("Found", nrow(ma_info), "unique meta-analyses\n\n")

# ==============================================================================
# Generate Reports for Each MA
# ==============================================================================

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
  
  # Define output filename using only MA identifier
  safe_id <- gsub("[^A-Za-z0-9_-]", "_", ma_id)
  output_file <- file.path(output_dir, sprintf("%s.html", safe_id))
  
  # Try to render the report
  tryCatch({
    quarto::quarto_render(
      input = input_qmd,
      execute_params = list(ma_no = ma_no),
      output_file = basename(output_file),
      output_format = "html",
      quiet = FALSE
    )
    
    # Move file to output directory if needed
    if (file.exists(basename(output_file))) {
      file.rename(basename(output_file), output_file)
    }
    
    successful <- successful + 1
    cat(sprintf("        ✓ Successfully created: %s\n", output_file))
    
  }, error = function(e) {
    failed <- failed + 1
    error_msg <- as.character(e)
    error_log[[as.character(ma_no)]] <<- list(
      ma_no = ma_no,
      ma_id = ma_id,
      error = error_msg
    )
    cat(sprintf("        ✗ ERROR: %s\n", error_msg))
  })
}

# ==============================================================================
# Summary
# ==============================================================================

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

cat("\nAll reports saved in:", normalizePath(output_dir), "\n")

# ==============================================================================
# Optional: Create Index File
# ==============================================================================

cat("\nCreating index file...\n")

index_content <- paste0(
  "<!DOCTYPE html>\n",
  "<html>\n",
  "<head>\n",
  "  <meta charset='utf-8'>\n",
  "  <title>Meta-Analysis Reports Index</title>\n",
  "  <style>\n",
  "    body { font-family: Arial, sans-serif; margin: 40px; }\n",
  "    h1 { color: #2c3e50; }\n",
  "    table { border-collapse: collapse; width: 100%; margin-top: 20px; }\n",
  "    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }\n",
  "    th { background-color: #3498db; color: white; }\n",
  "    tr:nth-child(even) { background-color: #f2f2f2; }\n",
  "    tr:hover { background-color: #ddd; }\n",
  "    a { color: #3498db; text-decoration: none; }\n",
  "    a:hover { text-decoration: underline; }\n",
  "    .summary { background-color: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px; }\n",
  "  </style>\n",
  "</head>\n",
  "<body>\n",
  "  <h1>Meta-Analysis Reports</h1>\n",
  "  <div class='summary'>\n",
  "    <strong>Summary:</strong> ", successful, " reports generated successfully",
  ifelse(failed > 0, paste0(" (", failed, " failed)"), ""), "\n",
  "  </div>\n",
  "  <table>\n",
  "    <tr>\n",
  "      <th>MA Number</th>\n",
  "      <th>Identifier</th>\n",
  "      <th>Sheet Name</th>\n",
  "      <th>Report</th>\n",
  "    </tr>\n"
)

for (i in 1:nrow(ma_info)) {
  ma_no <- ma_info$no[i]
  ma_id <- ma_info$identifier[i]
  sheet <- ma_info$sheet_name[i]
  safe_id <- gsub("[^A-Za-z0-9_-]", "_", ma_id)
  filename <- sprintf("%s.html", safe_id)
  
  index_content <- paste0(
    index_content,
    "    <tr>\n",
    "      <td>", ma_no, "</td>\n",
    "      <td>", ma_id, "</td>\n",
    "      <td>", sheet, "</td>\n",
    "      <td><a href='", filename, "'>View Report</a></td>\n",
    "    </tr>\n"
  )
}

index_content <- paste0(
  index_content,
  "  </table>\n",
  "  <p style='margin-top: 30px; color: #7f8c8d; font-size: 0.9em;'>\n",
  "    Generated on ", Sys.time(), "\n",
  "  </p>\n",
  "</body>\n",
  "</html>"
)

index_file <- file.path(output_dir, "index.html")
writeLines(index_content, index_file)
cat("Index file created:", index_file, "\n")

cat("\n✓ Done! Open", normalizePath(index_file), "to view all reports.\n")