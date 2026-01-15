#' Perform Meta-Analysis with Confidence Distribution Methods
#'
#' @description
#' Processes multiple meta-analyses from a dataset, computing confidence intervals
#' using various methods including Edgington's combination method with different
#' weighting schemes and optional Bayesian random-effects analysis.
#'
#' @param data A data frame containing meta-analysis data with effect estimates,
#'   standard errors, study identifiers, and meta-analysis groupings.
#' @param level Numeric. Confidence level for intervals (default: 0.95).
#' @param est_col Character. Name of column containing effect estimates (default: "logEst").
#' @param se_col Character. Name of column containing standard errors (default: "selogEst").
#' @param ma_id_col_num Character. Name of column containing *numeric* meta-analysis IDs
#'   for ordering (default: "no").
#' @param ma_id_col Character. Name of column containing meta-analysis identifiers
#'   (default: "identifier").
#' @param effect.measure Character. Name of column containing effect measure type
#'   (default: "effect.measure").
#' @param study Character. Name of column containing study names (default: "study").
#' @param parallel Logical. Whether to use parallel processing (default: FALSE) (can give problems in Windows).
#' @param n_cores Integer. Number of cores for parallel processing. If NULL,
#'   uses \code{detectCores() - 1} (default: NULL).
#' @param show_progress Logical. Whether to show progress bar (default: TRUE).
#' @param additional_info1_col Character. Name of first additional info column
#'   (default: "data_report").
#' @param additional_info2_col Character. Name of second additional info column
#'   (default: "sheet_name").
#' @param sign_threshold Numeric. Value used to check if the confidence interval contains the null hypothesis (default: 0).#'   
#' @param ... Additional arguments passed to \code{get_ma_results()}.
#'
#' @return An object of class "confMeta.full.list" containing a named list of
#'   meta-analysis results. Each element is an object of class "confMeta.full"
#'   with components:
#'   \describe{
#'     \item{inputs}{Dataframe of effect estimates and standard errors}
#'     \item{ma_id}{Meta-analysis identifier}
#'     \item{ma_id_number}{Numeric meta-analysis ID}
#'     \item{measure}{Effect measure type (RR, HR, OR, SMD, etc.)}
#'     \item{plot}{ggplot object with forest and p-value function plots}
#'     \item{ci}{Data frame with confidence intervals from all methods}
#'     \item{p_0}{Named vector of p-values testing null hypothesis}
#'     \item{width}{Named vector of confidence interval widths}
#'     \item{heterogeneity}{Data frame with heterogeneity statistics (Q, I2, Tau2 and Tau2 computed with bayesmeta package)}
#'     \item{significant}{Named logical vector indicating significance}
#'     \item{aucc_df}{Data frame with AUCC metrics}
#'     \item{ci_skewness}{Named vector of CI skewness measures}
#'     \item{data_skewness}{Weighted skewness of input data}
#'     \item{bayesian_model}{bayesmeta model object (if applicable)}
#'     \item{additional_info}{Named list of additional information}
#'   }
#'
#' @details
#' This function processes multiple meta-analyses in parallel or sequentially.
#' For each meta-analysis, it:
#' \itemize{
#'   \item Computes confidence intervals using Edgington's method with equal weights,
#'     inverse-SE weights, and inverse-variance weights
#'   \item Compares with fixed-effects and Hartung-Knapp methods
#'   \item Optionally performs Bayesian random-effects meta-analysis for RR, HR, OR, or SMD
#'   \item Generates forest plots and p-value function plots
#'   \item Calculates AUCC (Area Under Confidence Curve) metrics
#' }
#'
#' The function requires that each meta-analysis ID maps uniquely to a numeric ID.
#' Meta-analyses with errors are skipped with warnings.
#'
#' @section Bayesian Analysis:
#' When effect measure is RR, HR, OR, or SMD, Bayesian analysis uses half-normal
#' priors on tau with default scales:
#' \itemize{
#'   \item RR or HR: scale = 0.1
#'   \item OR: scale = 0.2
#'   \item SMD: scale = 0.3
#' }
#' These defaults follow recommendations Lilienthal, Jona, et al. "Bayesian random‐effects meta‐analysis with empirical heterogeneity priors for application in health technology assessment with very few studies." Research Synthesis Methods 15.2 (2024): 275-287.
#'
#'
#' @section Free Advices
#' If computing a large number of meta-analyisis, better to set \code{generate_plot = FALSE} and 
#' \code{include_bayesian = FALSE} to reduce the computational burden and the total memory used by the final object
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' library(dplyr)
#' data <- read.csv("meta_analysis_data.csv")
#'
#' # Run meta-analyses in parallel
#' results <- confMeta.full(
#'   data = data,
#'   level = 0.95,
#'   parallel = TRUE,
#'   n_cores = 4
#' )
#'
#' # Access results for first meta-analysis
#' results[[1]]$ci
#' results[[1]]$plot
#'
#' # Sequential processing for small datasets
#' results <- confMeta.full(
#'   data = data,
#'   parallel = FALSE
#' )
#' }
#'
#' @seealso \code{\link{get_ma_results}} for single meta-analysis processing
#'
#' @references
#' Lilienthal, Jona, et al. "Bayesian random‐effects meta‐analysis with empirical heterogeneity priors 
#' for application in health technology assessment with very few studies." Research Synthesis Methods 
#' 15.2 (2024): 275-287.
#'
#' @export

confMeta.full <- function(data,
                   level = 0.95,
                   est_col = "logEst",
                   se_col = "selogEst",
                   ma_id_col_num = "no",
                   ma_id_col = "identifier",
                   effect.measure = "effect.measure",
                   study = "study",
                   parallel = FALSE,
                   n_cores = NULL,
                   show_progress = TRUE,
                   additional_info1_col = "data_report",
                   additional_info2_col = "sheet_name",
                   sign_threshold = 0,
                   ...) {
  
  if (!is.numeric(data[[ma_id_col_num]])) {
    stop("Column ", ma_id_col_num, " must be numeric.")
  }
  
  # ---- Ordering the df by ma_id_col ----
  o <- order(data[[ma_id_col_num]]) 
  data <- data[o, , drop = FALSE]
  
  
  # ---- Validation ----
  
  # Check required columns exist
  required_cols <- c(ma_id_col, est_col, se_col, effect.measure , study, ma_id_col_num)
  
  # check if we have additional_info 
  if (!is.null(additional_info1_col)) required_cols <- c(required_cols, additional_info1_col)
  if (!is.null(additional_info2_col)) required_cols <- c(required_cols, additional_info2_col)
  
  required_cols <- unique(required_cols)
  
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate level
  if (level <= 0 || level >= 1) {
    stop("Confidence level must be between 0 and 1")
  }
  
  # ---- Prepare Data  ----
  
  
  # Subset to only needed columns
  df <- data[, required_cols, drop = FALSE]
  
  
  # Get unique meta-analysis IDs and names
  ids <- unique(df[[ma_id_col]])
  n0s <- unique(df[[ma_id_col_num]])
  
  # Check alignment
  if (length(ids) != length(n0s)) {
    stop("Number of unique MA number IDs does not match number of unique MA IDs ",
         "Fix that to run this function")
  }
  
  
  #maybe have same length but not 1-1 mapping:
  pairs <- unique(df[, c(ma_id_col, ma_id_col_num), drop = FALSE])
  if (any(duplicated(pairs[[ma_id_col]])) || any(duplicated(pairs[[ma_id_col_num]]))) {
    stop("MA id and MA number are not in a 1-1 mapping.")
  }
  
  
  
  
  #  ---- Parallel Processing -----
  
  if (parallel) {
    # Determine number of cores
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 1)
    }
    
    # Don't use more cores than meta-analyses
    n_cores <- min(n_cores, length(ids))
    
    if (show_progress) {
      message(paste0("Processing ", length(ids), " meta-analyses using ", n_cores, " cores..."
      ))
    }
  }
  
  
  
  # ---- Parallel process ----
  # Choose processing method
  if (parallel && length(ids) > 1) {
    
    # Parallel processing with progress bar
    out <- pbapply::pblapply(
      X = ids,
      cl = n_cores,
      FUN = process_single_ma,
      
      # parameter for process_single_ma: 
      df = df,
      id_col_name= ma_id_col,
      n0 = ma_id_col_num,
      level = level,
      est_col = est_col, 
      se_col = se_col,
      study = study,
      effect.measure = effect.measure,
      additional_info1_col = additional_info1_col,
      additional_info2_col = additional_info2_col,
      ...
    )
  } else {
    # Sequential processing
    if (show_progress && length(ids) > 1) {
      out <- pbapply::pblapply(
        X = ids,
        FUN = process_single_ma,
        df = df,
        id_col_name= ma_id_col,
        n0 = ma_id_col_num,
        level = level,
        est_col = est_col, 
        se_col = se_col,
        study = study,
        effect.measure = effect.measure,
        additional_info1_col = additional_info1_col,
        additional_info2_col = additional_info2_col,
        ...
      )
    } else {
      out <- lapply(
        X = ids,
        FUN = process_single_ma,
        df = df,
        id_col_name= ma_id_col,
        n0 = ma_id_col_num,
        level = level,
        est_col = est_col, 
        se_col = se_col,
        study = study,
        effect.measure = effect.measure,
        additional_info1_col = additional_info1_col,
        additional_info2_col = additional_info2_col,
        ...
      )
    }
  }
  
  # ---- Results -----
  
  
  # Name the output list
  names(out) <- ids
  
  # Remove NULL entries (failed analyses)
  failed <- sapply(out, is.null)
  if (any(failed)) {
    warning(sprintf("%d meta-analysis(es) failed to process", sum(failed)))
    out <- out[!failed]
  }
  
  class(out) <- "confMeta.full.list"
  return(out)
}








#' Process Single Meta-Analysis
#'
#' @description
#' Internal function to process a single meta-analysis. Called by \code{confMeta.full()}
#' for each unique meta-analysis ID.
#'
#' @param id_col_name Character. Name of the meta-analysis ID column.
#' @param id Character/Numeric. The specific meta-analysis ID to process.
#' @param df Data frame containing all meta-analysis data.
#' @param n0 Character. Name of the numeric ID column.
#' @param est_col Character. Name of estimates column.
#' @param se_col Character. Name of standard errors column.
#' @param level Numeric. Confidence level.
#' @param study Character. Name of study identifier column.
#' @param effect.measure Character. Name of effect measure column.
#' @param additional_info1_col Character. First additional info column (optional).
#' @param additional_info2_col Character. Second additional info column (optional).
#' @param sign_threshold Numeric. Value used to check if the confidence interval contains the null hypothesis (default: 0).
#' @param ... Additional arguments passed to \code{get_ma_results()}.
#'
#' @return An object of class "confMeta.full" or NULL if processing fails.
#'
#' @keywords internal
#' @noRd



process_single_ma <- function(id, df, id_col_name, n0, est_col, se_col, level, study, effect.measure,
                              additional_info1_col = NULL, 
                              additional_info2_col = NULL, sign_threshold = 0,
                              ...) {
  # Subset data just from this meta-analysis
  subset_data <- df[df[[id_col_name]] == id, , drop = FALSE]
  
  
  # Check if subset has data
  if (nrow(subset_data) == 0) {
    warning(sprintf("No data found for meta-analysis ID: %s (number: %s)", id, n0))
    return(NULL)
  }
  
  
  n0_unique <- unique(subset_data[[n0]])
  if (length(n0_unique) != 1L) {
    stop("Non-unique ", n0, " within ma_id=", id)
  }
  n0_unique <- n0_unique[[1]]
  
  
  
  #Build additional_info sublist 
  add <- list() #this will be the list we add inside each MA
  
  if (!is.null(additional_info1_col)) {
    u1 <- unique(subset_data[[additional_info1_col]])
    if (length(u1) == 1) {
      add[[additional_info1_col]] <- as.character(u1[1])
    }
  }
  
  if (!is.null(additional_info2_col)) {
    u2 <- unique(subset_data[[additional_info2_col]])
    if (length(u2) == 1) {
      add[[additional_info2_col]] <- as.character(u2[1])
    }
  }
  
  additional_info <- if (length(add) > 0L) add else NULL
  
  
  
  # Run analysis with error handling
  tryCatch(
    {
      get_ma_results(data = subset_data, 
                     level = level, 
                     est_col = est_col,
                     se_col = se_col, 
                     study_name = study,
                     effect_measure = effect.measure,   
                     additional_info = additional_info,
                     ma_id = id,
                     ma_id_number = n0_unique,
                     ...)
    },
    error = function(e) {
      warning(sprintf("Error processing meta-analysis ID %s, %s: %s", 
                      id, n0_unique,  e$message))
      return(NULL) #we set this meta analysis null if it fails 
    }
  )
}



















#' Compute Meta-Analysis Results with Multiple CI Methods
#'
#' @description
#' Performs a *single* meta-analysis using confidence distribution methods,
#' computing confidence intervals via Edgington's combination with various
#' weighting schemes, and optionally Bayesian random-effects analysis.
#'
#' @param data Data frame containing effect estimates and standard errors
#'   for a single meta-analysis.
#' @param level Numeric. Confidence level (default: 0.95).
#' @param est_col Character. Column name for effect estimates (default: "logEst").
#' @param se_col Character. Column name for standard errors (default: "selogEst").
#' @param methods_to_exclude Character vector. Methods to exclude from results
#'   (default: c("Random effects", "Henmi & Copas")).
#' @param reference_methods Character vector. Reference methods for plotting
#'   (default: c("fe", "hk")).
#' @param plot_types Character vector. Types of plots to generate: "p" for
#'   p-value functions, "forest" for forest plots (default: c("p", "forest")).
#' @param study_name Character. Column name for study identifiers (default: "study").
#' @param effect_measure Character. Column name for effect measure type
#'   (default: "effect.measure").
#' @param ma_id Character/Numeric. Meta-analysis identifier (default: NULL).
#' @param ma_id_number Numeric. Meta-analysis numeric ID (default: NULL).
#' @param additional_info Named list. Additional metadata to store (default: NULL).
#' @param generate_plot Logical. Whether to generate plots (default: TRUE).
#' @param include_bayesian Logical. Whether to perform Bayesian analysis
#'   (default: TRUE).
#' @param tau_prior_scale_rr Numeric. Prior scale for tau when effect measure
#'   is RR or HR (default: 0.1).
#' @param tau_prior_scale_or Numeric. Prior scale for tau when effect measure
#'   is OR (default: 0.2).
#' @param tau_prior_scale_smd Numeric. Prior scale for tau when effect measure
#'   is SMD (default: 0.3).
#' @param sign_threshold Numeric. Value used to check if the confidence interval contains the null hypothesis (default: 0).
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class "confMeta.full" (see \code{\link{confMeta.full}} for details).
#'
#' @details
#' This function is typically called by \code{confMeta.full()} but can be used standalone
#' for analyzing a single meta-analysis.
#'
#' Methods computed:
#' \itemize{
#'   \item Edgington with equal weights
#'   \item Edgington with inverse-SE weights
#'   \item Edgington with inverse-variance weights
#'   \item Fixed-effects (comparison)
#'   \item Hartung-Knapp (comparison)
#'   \item Bayesian random-effects (optional, for RR/HR/OR/SMD only)
#' }
#'
#' @section Prior Specifications:
#' Bayesian analysis uses half-normal priors on the heterogeneity parameter tau.
#' Default scales are based on : Lilienthal, Jona, et al. "Bayesian random‐effects meta‐analysis with empirical heterogeneity priors 
#' for application in health technology assessment with very few studies.":
#' \itemize{
#'   \item Log risk ratios (RR) or log hazard ratios (HR): scale = 0.1
#'   \item Log odds ratios (OR): scale = 0.2  
#'   \item Standardized mean differences (SMD): scale = 0.3
#' }
#'
#' @examples
#' \dontrun{
#' # Single meta-analysis
#' data_subset <- data[data$identifier == "MA001", ]
#' 
#' result <- get_ma_results(
#'   data = data_subset,
#'   level = 0.95,
#'   include_bayesian = TRUE
#' )
#' 
#' # View confidence intervals
#' print(result$ci)
#' 
#' # Display plots
#' print(result$plot)
#' }
#'
#' @export

get_ma_results <- function(data, 
                           level = 0.95,
                           est_col = "logEst", se_col = "selogEst",
                           methods_to_exclude = c("Random effects", "Henmi & Copas"),
                           reference_methods = c("fe", "hk"),
                           plot_types = c("p", "forest"),
                           study_name ="study", 
                           effect_measure = "effect.measure",
                           ma_id = NULL,
                           ma_id_number = NULL,
                           additional_info = NULL, # should be a NAMED list
                           generate_plot = TRUE,
                           include_bayesian = TRUE,
                           tau_prior_scale_rr = 0.1,
                           tau_prior_scale_or = 0.2,
                           tau_prior_scale_smd = 0.3,
                           sign_threshold = 0,
                           ...) {
  
  # ---- Take values ----
  estimates <- data[[est_col]]
  SEs <- data[[se_col]]
  study_names <- data[[study_name]]
  conf_level <- level
  meas <- unique(data[[effect_measure]]) 
  meas <- as.character(meas)
  
  
  # Validate single effect measure
  stopifnot(
    "Only one effect measure is allowed" = length(meas) == 1L
  )
  
  
  # I deleted the heterogeneity estimate, if want to add, check old Quarto file
  
  # ---- Define Weighs ----

  w_equal <- rep(1, length(SEs))
  w_inv_se <- 1 / SEs
  w_inv_se2 <- 1 / (SEs^2)
  
  # ----- Setup Methods ----

  # Create base functions
  p_edg <- p_edgington_w 
  p_edg_w1 <- p_edgington_w
  p_edg_w2 <- p_edgington_w
  
  # Set heterogeneity parameter
  formals(p_edg)$heterogeneity <- "none"
  formals(p_edg_w1)$heterogeneity <- "none"
  formals(p_edg_w2)$heterogeneity <- "none"
  
  # Define methods list
  methods <- list(
    list(name = "Edgington",          fun = p_edg,   w = w_equal),
    list(name = "Edgington (1/SE)",   fun = p_edg_w1, w = w_inv_se),
    list(name = "Edgington (1/SE^2)", fun = p_edg_w2, w = w_inv_se2)
  )
  

  # ---- Run confMeta for Each Method ----

  cms <- lapply(methods, function(mdef) {
    confMeta(
      estimates = estimates,
      SEs = SEs,
      w = mdef$w,
      study_names = study_names,
      fun = mdef$fun,
      fun_name = mdef$name,
      conf_level = conf_level
    )
  })
  names(cms) <- vapply(methods, `[[`, character(1L), "name")
  
  # ---- Extract Confidence Intervals ----
  
  #Extract individual cis, we just need to exctract them from a random method since they are all the same
  ci_individual <- as.data.frame( cms[[1]][["individual_cis"]] ) 

  
  ci_individual <- ci_individual %>% 
    mutate (
      width = upper - lower,
      significant = !(lower < sign_threshold & upper > sign_threshold)
    )

  
  
  # Extract joint CIs from confMeta objects
  ci_new <- lapply(cms, `[[`, i = "joint_cis")
  
  # Extract comparison CIs (fixed effects, HK, etc.)
  ci_comparison <- lapply(
    seq_len(nrow(cms[[1]]$comparison_cis)), #extract it just fromt he first edgignton object, they are the same 
    function(x) {
      out <- cms[[1]]$comparison_cis[x, , drop = FALSE]
      rownames(out) <- NULL
      out
    }
  )
  names(ci_comparison) <- rownames(cms[[1]]$comparison_cis)
  
  # Combine all CIs
  ci_all <- append(ci_new, ci_comparison)
  
  # ---- Create CI Summary Data Frame ----

  ci_out <- .create_ci_dataframe(ci_all, sign_threshold = sign_threshold)
  
  # Filter out unwanted methods
  ci_out <- ci_out %>%
    filter(!method %in% methods_to_exclude)
  

  # ---- Add p-vals -----

  ci_out <- .add_p_values(ci_out, cms, methods_to_exclude)
  

  # ---- Add AUCC -----

  ci_out <- .add_aucc_metrics(ci_out, cms)
  
  # ---- Add Point Estimates and CI Skewness ----

  ci_out <- .add_estimates_and_skewness(ci_out, cms)
  
  # ---- Bayesian Analysis (if TRUE) -----
  
  #Bayesian prior supported just for RR, HR, OR, SMD
  if (include_bayesian) {
    if (!meas %in% c("RR", "HR", "OR", "SMD")) {
      warning("Bayesian analysis only implemented for RR, OR, and SMD. Skipping Bayesian computation.")
      include_bayesian <- FALSE
    }
  }
  
  
  if (include_bayesian) {
    bayesian_results <- .run_bayesian_analysis(
      estimates = estimates,
      SEs = SEs,
      meas = meas,
      tau_prior_scale_rr = tau_prior_scale_rr,
      tau_prior_scale_or = tau_prior_scale_or,
      tau_prior_scale_smd = tau_prior_scale_smd,
      sign_threshold = sign_threshold
    )
    
    # Add Bayesian results to ci_out
    ci_out <- rbind(ci_out, bayesian_results$ci_row)
    
    # Store Bayesian model object
    bm <- bayesian_results$model
    tau2_bayes <- bayesian_results$tau2
  } else {
    bm <- NULL
    tau2_bayes <- NA
  }
  
  # ----- Calculate Data-Level Skewness  -----

  data_skewness <- .calculate_weighted_skewness(estimates, SEs)
  

  # ----- Generate Plots -----

   if (generate_plot){
  plot_args <- append(
    cms,
    list(
      reference_methods = reference_methods,
      type = plot_types
    )
  )
  
  if (include_bayesian && !is.null(bm)) {
    plot_args$bayesmeta <- bm
  }
  
  plots <- do.call("autoplot", plot_args)
   } else {
     plots <- NULL
   }

  # ---- Prepare Output ----
  # Input matrix
  inputs <- data.frame(
    estimate = estimates, 
    SE = SEs, 
    row.names = study_names
  )
  
  inputs <- cbind(inputs, ci_individual)
  
  # Extract vectors
  width <- setNames(ci_out$width, ci_out$method)
  significant <- setNames(ci_out$significant, ci_out$method)
  p_0 <- setNames(ci_out$p_0, ci_out$method)
  ci_skewness <- setNames(ci_out$ci_skewness, ci_out$method)
  estimates <- setNames(ci_out$estimate, ci_out$method)
  
  # Tau-squared estimates
  tau2_bayes <- data.frame("Bayesian_tau2" = tau2_bayes)
  
  #heterogeneity df (the same for each method, just extract the first)
  heterogeneity <- cms[[1]]$heterogeneity
  
  #add the bayesian tau to the heterogeneity
  heterogeneity <- cbind(heterogeneity,tau2_bayes) %>%
    mutate (
      significant_pval = (p_Q <= (1-level))
    )
  
  # AUCC data frame
  aucc_df <- ci_out[ci_out$method %in% names(cms), c("method", "aucc", "aucc_ratio")]
  
  out <- list(
    inputs = inputs,
    ma_id = ma_id,
    ma_id_number = ma_id_number, 
    estimates = estimates,
    measure = meas,
    plot = plots,
    ci = ci_out,
    p_0 = p_0,
    width = width,
    heterogeneity = heterogeneity, 
    significant = significant,
    aucc_df = aucc_df,
    ci_skewness = ci_skewness,
    data_skewness = data_skewness,
    bayesian_model = if (include_bayesian) bm else NULL
  )
  
  # additional_info: must be a named list (scalars already enforced by confMeta.full)
  
  if (!is.null(additional_info)) {
    if (!is.list(additional_info)) stop("additional_info must be a list.")
    if (is.null(names(additional_info)) || any(names(additional_info) == "")) {
      stop("additional_info must be a named list.")
    }
    if (length(additional_info) > 0L) out$additional_info <- additional_info
  }
  
  
  
  class(out) <- "confMeta.full"
  out
}









# ==============================================================================
# Internal Helper Functions
# ==============================================================================

#' Create CI DataFrame from List
#' @keywords internal
#' @noRd
.create_ci_dataframe <- function(ci_list, sign_threshold = 0) {
  
  #create list of df
  df_list <- lapply(names(ci_list), function(m_name) {
    m <- ci_list[[m_name]] #extract singular df
    
    # NOTE: i write it to handle df with any number of rows, even though not necessary 
    lower <- m[, 1L]
    upper <- m[, 2L]
    
    data.frame(
      method      = m_name,
      lower       = lower,
      upper       = upper,
      width       = upper - lower,
      significant = !(lower < sign_threshold & upper > sign_threshold),
      stringsAsFactors = FALSE
    )
  })
  
  #from list of df to single df 
  df <- do.call("rbind", df_list)
  
  return(df)
}



#' Add P-Values to CI DataFrame
#' @keywords internal
#' @noRd
.add_p_values <- function(ci_df, cms, methods_to_exclude) {
  # Extract p-values from confMeta objects
  p_0_new <- vapply(cms, function(x) x[["p_0"]][, "y"], double(1L))
  p_0_old <- cms[[1]][["comparison_p_0"]][, "y"]
  p_0 <- c(p_0_new, p_0_old)
  
  # Remove unwanted methods
  p_0 <- p_0[!(names(p_0) %in% methods_to_exclude)]
  
  # Merge into ci_df
  ci_df <- merge(
    ci_df,
    data.frame(
      method = names(p_0),
      p_0 = p_0,
      stringsAsFactors = FALSE
    ),
    by = "method", all.x = TRUE, sort = FALSE
  )
  
  return(ci_df)
}



#' Add P-Values to CI DataFrame
#' @keywords internal
#' @noRd
.add_aucc_metrics <- function(ci_df, cms) {
  aucc_df <- data.frame(
    method = names(cms),
    aucc = vapply(cms, `[[`, numeric(1L), "aucc"),
    aucc_ratio = vapply(cms, `[[`, numeric(1L), "aucc_ratio"),
    stringsAsFactors = FALSE
  )
  
  merge(ci_df, aucc_df, by = "method", all.x = TRUE, sort = FALSE)
}




#' Add Point Estimates and CI Skewness
#' @keywords internal
#' @noRd
.add_estimates_and_skewness <- function(ci_df, cms) {
  # Extract central estimates from confMeta objects
  m_values <- vapply(cms, function(x) x$p_max[, "x"], numeric(1L))
  names(m_values) <- names(cms)
  
  # Merge estimates
  ci_df <- merge(
    ci_df,
    data.frame(
      method = names(m_values),
      estimate = m_values,
      stringsAsFactors = FALSE
    ),
    by = "method", all.x = TRUE, sort = FALSE
  )
  
  # For symmetric methods, estimate is midpoint
  ci_df$estimate <- ifelse(
    is.na(ci_df$estimate),
    (ci_df$upper + ci_df$lower) / 2,
    ci_df$estimate
  )
  
  # Calculate CI skewness
  ci_df$ci_skewness <- with(ci_df, {
    (upper + lower - 2 * estimate) / (upper - lower)
  })
  
  return(ci_df)
}




#' Run Bayesian Meta-Analysis
#' @keywords internal
#' @noRd
.run_bayesian_analysis <- function(estimates, SEs, meas, 
                                   tau_prior_scale_rr, tau_prior_scale_or,tau_prior_scale_smd, point_estimate = "median", sign_threshold = sign_threshold) {
  # Set prior based on effect measure
  # Note: for two studies, they dont suggest to use this
  tau_prior <- if (meas == "RR" | meas == "HR") {
    function(tau) bayesmeta::dhalfnormal(tau, scale = tau_prior_scale_rr)
  } else if (meas == "OR") {
    function(tau) bayesmeta::dhalfnormal(tau, scale = tau_prior_scale_or)
  } else if (meas == "SMD") {
    function(tau) bayesmeta::dhalfnormal(tau, scale = tau_prior_scale_smd)
  } else {
    stop("Effect measure must be 'RR' or 'HR' or 'OR' or 'SMD' for Bayesian analysis")
  }
  
  # Run Bayesian meta-analysis
  bm <- bayesmeta(
    y = estimates,
    sigma = SEs,
    tau.prior = tau_prior
  )
  
  # Extract summary statistics
  bm_lower <- bm$summary["lower", "mu"]
  bm_upper <- bm$summary["upper", "mu"]
  bm_est <- bm$summary[point_estimate, "mu"]
  bm_tau2 <- bm$summary["median", "tau"]^2
  
  # Calculate derived metrics
  bm_width <- bm_upper - bm_lower
  bm_significant <- !(sign_threshold > bm_lower & sign_threshold < bm_upper)
  bm_ci_skewness <- (bm_upper + bm_lower - 2 * bm_est) / (bm_upper - bm_lower)
  
  # Create row for ci_out
  bm_row <- data.frame(
    method = "Bayesmeta",
    lower = bm_lower,
    upper = bm_upper,
    width = bm_width,
    estimate = bm_est,
    ci_skewness = bm_ci_skewness,
    significant = bm_significant,
    p_0 = NA,
    aucc = NA,
    aucc_ratio = NA,
    stringsAsFactors = FALSE
  )
  
  return(list(
    model = bm,
    ci_row = bm_row,
    tau2 = bm_tau2
  ))
}




#' Calculate Weighted Skewness
#' @keywords internal
#' @noRd
.calculate_weighted_skewness <- function(est, se) {
  w <- 1 / se^2
  theta_bar <- sum(w * est) / sum(w)
  num <- sum(w * (est - theta_bar)^3)
  den <- (sum(w * (est - theta_bar)^2)^(3/2)) / sqrt(sum(w))
  num / den
}




