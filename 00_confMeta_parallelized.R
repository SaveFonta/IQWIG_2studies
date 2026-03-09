

####################################################################
######       00_confMeta_parallelized.R                        #####
####################################################################


# Processes multiple meta-analyses from a dataset, computing confMeta objects 
# using Edgington with different weighting schemes, baselines and optional Bayesian random-effects analysis (HEAVY).

# Arguments:
#   data:                 DataFrame containing meta-analysis data with estimates, SEs, IDs, and groupings.
#   level:                Confidence level for intervals
#   est_col:              Column name for effect estimates
#   se_col:               Column name for standard errors
#   ma_id_col_num:        Column name for *numeric* meta-analysis IDs (ordering) (default: "no").
#   ma_id_col:            Column name for meta-analysis identifiers (default: "identifier").
#   effect.measure:       Column name for effect measure type (e.g. RD, MD, OR....).
#   study:                Column name for study names
#   parallel:             Whether to use parallel processing (default: FALSE).
#   n_cores:              Number of cores. If NULL, uses detectCores() - 1.
#   additional_info1_col: Name of first additional info column (default: "data_report").
#   additional_info2_col: Name of second additional info column (default: "sheet_name").
#   sign_threshold:       Value to check if CI contains null hypothesis (default: 0). 
#   MH:                   If TRUE, performs Mantel-Haenszel FE. Requires cols ai, bi, ci, di, n1i, n2i.
#   ... :                 Additional args passed to confMeta().
#
# Returns:
#   An object of class "confMeta.full.list" (named list). Each element is "confMeta.full" with components:
#     - inputs:           Dataframe of effect estimates and SEs.
#     - ma_id:            Meta-analysis identifier.
#     - ma_id_number:     Numeric meta-analysis ID.
#     - measure:          Effect measure type (RR, HR, OR, SMD, etc.).
#     - plot:             ggplot object with forest and p-value function plots.
#     - ci:               Data frame with confidence intervals from all methods.
#     - p_0:              Named vector of p-values testing null hypothesis.
#     - width:            Named vector of CI widths.
#     - heterogeneity:    Data frame with Q, I2, Tau2 (DL & Bayesian).
#     - significant:      Named logical vector indicating significance.
#     - aucc_df:          Data frame with AUCC metrics.
#     - ci_skewness:      Named vector of CI skewness measures.
#     - data_skewness:    Weighted skewness of input data.
#     - bayesian_model:   bayesmeta model object (if applicable).
#     - additional_info:  Named list of additional information.
#
# Bayesian Analysis Priors (based on Lilienthal et al., 2024):
#   Uses half-normal priors on tau:
#   - RR or HR: scale = 0.1
#   - OR:       scale = 0.2
#   - SMD:      scale = 0.3
#
# Note:
#   For large numbers of meta-analyses, set generate_plot = FALSE and 
#   include_bayesian = FALSE to save memory and time.
# ==============================================================================




 
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
                   additional_info1_col = "data_report",
                   additional_info2_col = "sheet_name",
                   sign_threshold = 0,
                   MH = FALSE,
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
  if (MH == TRUE) {
    MH_cols <- c("ai", "bi", "ci", "di", "n1i", "n2i", "MH_flag")
    
    # Check if MH columns exist
    missing_MH_cols <- setdiff(MH_cols, names(data))
    if (length(missing_MH_cols) > 0) {
      stop("When MH = TRUE, the following columns are required: ", 
           paste(missing_MH_cols, collapse = ", "))
    }
    
    df <- data[, c(required_cols, MH_cols), drop = FALSE]
  } else {
    df <- data[, required_cols, drop = FALSE]
  }
  
  
  # Get unique meta-analysis IDs and names
  # Fast way, using split 
  id_vec <- as.character(df[[ma_id_col]]) 
  if (anyNA(id_vec)) stop("Missing meta-analysis IDs in column ", ma_id_col, ".")
  
  #ids <- unique(id_vec)
  #this is faster and does the same:
  id_factor <- factor(id_vec, levels = unique(id_vec))
  ids <- levels(id_factor)
  
  idx_by_id <- split(seq_len(nrow(df)), id_vec)
  

  # numeric ids 
  first_occurrence <- match(ids, id_vec)
  n0s <- df[[ma_id_col_num]][first_occurrence]
  
  # Validation: Check 1-1 mapping more efficiently
  id_mapping <- df[first_occurrence, c(ma_id_col, ma_id_col_num), drop = FALSE]
  
  if (anyDuplicated(id_mapping[[ma_id_col]]) || anyDuplicated(id_mapping[[ma_id_col_num]])) {
    stop("MA id and MA number are not in a 1-1 mapping.")
  }
  

  
  
  # ---- Process meta-analyses ----
  # CHANGED: Use future.apply instead of pbapply for parallel
  # ---- Process meta-analyses ----
  
  if (parallel && length(ids) > 1) {
    
    # first we configure the parallel plan
    workers_to_use <- if (!is.null(n_cores)) n_cores else parallelly::availableCores()
    future::plan(future::multisession, workers = workers_to_use)
    
    # Parallel Execution with progress bar
    progressr::with_progress({
      
      p <- progressr::progressor(along = ids)
      
      out <- future.apply::future_lapply(
        X = ids, 
        FUN = function(x, ...) {
          res <- process_single_ma(x, ...)
          p() # move the progress bar
          return(res)
        },
        
        # Data & Arguments
        df = df,
        idx_by_id = idx_by_id, 
        id_col_name = ma_id_col,
        n0 = ma_id_col_num,
        level = level,
        est_col = est_col, 
        se_col = se_col,
        study = study,
        sign_threshold = sign_threshold,
        effect.measure = effect.measure,
        additional_info1_col = additional_info1_col,
        additional_info2_col = additional_info2_col,
        MH = MH,
        ...,
        
        # Future Settings
        future.seed = TRUE      # uses the same seed each core (not necessary here actually but whatever)
      )
      
    }, handlers = progressr::handler_txtprogressbar(char = "=")) 
    
    # Force back to sequential
    future::plan(future::sequential)
    
  } else {
    
    # SEQUENTIAL
    out <- pbapply::pblapply(
      X = ids,
      FUN = process_single_ma,
      df = df,
      idx_by_id = idx_by_id, 
      id_col_name = ma_id_col,
      n0 = ma_id_col_num,
      level = level,
      est_col = est_col, 
      se_col = se_col,
      study = study,
      sign_threshold = sign_threshold, 
      effect.measure = effect.measure,
      additional_info1_col = additional_info1_col,
      additional_info2_col = additional_info2_col,
      MH = MH,
      ...
    )
  }
  
  # ---- Results -----
  
  
  # Name the output list
  names(out) <- ids
  
  # Remove NULL entries (failed analyses)
  failed <- lengths(out) == 0L
  if (any(failed)) {
    warning(sprintf("%d meta-analysis(es) failed to process", sum(failed)))
    out <- out[!failed]
  }
  
  class(out) <- "confMeta.full.list"
  return(out)
}







#  ---- process_single_ma ----
# wrapper of get_ma_result to subset data and run some checks



process_single_ma <- function(id, df, idx_by_id, id_col_name, n0, est_col, se_col, level, study, effect.measure,
                              additional_info1_col = NULL, 
                              additional_info2_col = NULL, sign_threshold = 0, MH = FALSE, 
                              ...) {
  # Subset data just from this meta-analysis
  idx <- idx_by_id[[id]]
  if (is.null(idx)) {
    warning(sprintf("No data found for meta-analysis ID: %s", id))
    return(NULL)
  }
  subset_data <- df[idx, , drop = FALSE]  
  
  
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
  
  
  #check if this line has TRUE in MH

  if (isTRUE(MH) &&
      "MH_flag" %in% names(subset_data) &&
      identical(subset_data$MH_flag[1], FALSE)) {
    MH <- FALSE
  } #assuming escalc doing a good job 
  
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
                     sign_threshold= sign_threshold,
                     MH = MH,
                     ...)
    },
    error = function(e) {
      warning(sprintf("Error processing meta-analysis ID %s, %s: %s", 
                      id, n0_unique,  e$message))
      return(NULL) #we set this meta analysis null if it fails 
    }
  )
}



















# ---- get_ma_results ----

# more useful function, gives an object confMeta.full, which is just an "augmented" confMeta object
# unlike a confmeta object it involves many methods together in the same object.
# It is called for each meta analysis, in the end we will have one big confMeta.full.list object, and its elements
# will be one confmeta.full each MA 

get_ma_results <- function(data, 
                           level = 0.95,
                           est_col = "logEst", se_col = "selogEst",
                           methods_to_exclude =  c("Random effects", "Henmi & Copas"),
                           reference_methods = c("fe", "hk"), #those are the reference methods for the plot
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
                           MH = FALSE, 
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
  
  # create 2x2 table (df)
  # create 2x2 table (df)
  if (MH == TRUE) {
    # Check required columns
    MH_cols <- c("ai", "bi", "ci", "di", "n1i", "n2i")
    missing_cols <- setdiff(MH_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("When MH = TRUE, the following columns are required: ", 
           paste(missing_cols, collapse = ", "))
    }
    
    table_2x2 <- data.frame(
      ai  = data[["ai"]],
      bi  = data[["bi"]],
      ci  = data[["ci"]],
      di  = data[["di"]],
      n1i = data[["n1i"]],
      n2i = data[["n2i"]]
    )
    
    # Validate values ì
    if (any(table_2x2 < 0, na.rm = TRUE)) {
      stop("2x2 table values must be non-negative")
    }
  } else {
    table_2x2 <- NULL  # Explicitly set to NULL when not used
  }
    
  
  # ---- Run confMeta for Each Method ----
  
  base_args <- list(
    estimates = estimates,
    SEs = SEs,
    study_names = study_names,
    conf_level = conf_level
  )
  
  if (MH == TRUE) {
    base_args$MH <- TRUE
    base_args$table_2x2 <- table_2x2
    base_args$measure <- meas
  }
  
  cms <- lapply(methods, function(mdef) {
    args <- c(base_args, list(
      w = mdef$w,
      fun = mdef$fun,
      fun_name = mdef$name
    ), list(...))
    do.call(confMeta, args)
  })
  
  names(cms) <- vapply(methods, `[[`, character(1L), "name")
  

  # ---- Extract Confidence Intervals ----
  
  #Extract individual cis, extract it from Edgington(but same for every method)
  ci_individual <- as.data.frame( cms[["Edgington"]][["individual_cis"]] ) 
  ci_individual$width <- ci_individual$upper - ci_individual$lower
  ci_individual$significant <- !(ci_individual$lower <= sign_threshold & 
                                   ci_individual$upper >= sign_threshold)
  


  # Extract joint CIs from confMeta objects
  ci_new <- lapply(cms, `[[`, i = "joint_cis")
  
  # Extract comparison CIs (fixed effects, HK, etc.)
  ci_comparison <- lapply(
    seq_len(nrow(cms[["Edgington"]]$comparison_cis)), #extract it just from the Edgington! Important if use MH = TRUE
    function(x) {
      out <- cms[["Edgington"]]$comparison_cis[x, , drop = FALSE]
      rownames(out) <- NULL
      out
    }
  )
  names(ci_comparison) <- rownames(cms[["Edgington"]]$comparison_cis)
  
  # Combine all CIs
  ci_all <- append(ci_new, ci_comparison)
  
  # ---- Create CI Summary Data Frame ----

  ci_out <- .create_ci_dataframe(ci_all, sign_threshold = sign_threshold)
  
  # Filter out unwanted methods (if NULL is passed, then give all comparison methods)
  if (!is.null(methods_to_exclude) && length(methods_to_exclude) > 0) {
    ci_out <- ci_out %>%
      filter(!method %in% methods_to_exclude)
  }

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
     
     # did we set xlim in the dots ? 
     dot_argums <- list(...)
     
     if ("xlim" %in% names(dot_argums)) {
       use_xlim <- dot_argums$xlim #if yes use it
     } else {
       # otw, compute it using individual studies CIs:
       
       # I decided to use the min (lower CIs) and max (upper CIS) and then add 10%
       
       min_lower <- min(ci_individual$lower, na.rm = TRUE)
       max_upper <- max(ci_individual$upper, na.rm = TRUE)
       
       rng <- max_upper - min_lower
       if (rng == 0) rng <- 1.0 # don't sure it can happen
       
       margin <- rng * 0.10 #arbitrary number for visualisation 
       
       use_xlim <- c(min_lower - margin, max_upper + margin)
     }
     
     
     
  plot_args <- append(
    cms,
    list(
      reference_methods = reference_methods,
      type = plot_types,
      xlim = use_xlim  
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
    bayesian_model = if (include_bayesian) bm else NULL,
    table_2x2 = if (MH) table_2x2 else NULL
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

# Create CI DataFrame from List

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
      significant = !(lower <= sign_threshold & upper >= sign_threshold),
      stringsAsFactors = FALSE
    )
  })
  
  #from list of df to single df 
  df <- do.call("rbind", df_list)
  
  return(df)
}



# Add P-Values to CI DataFrame
.add_p_values <- function(ci_df, cms, methods_to_exclude) {
  # Extract p-values from confMeta objects
  p_0_new <- vapply(cms, function(x) x[["p_0"]][, "y"], double(1L))
  p_0_old <- cms[["Edgington"]][["comparison_p_0"]][, "y"]
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



# Add P-Values to CI DataFrame

.add_aucc_metrics <- function(ci_df, cms) {
  aucc_df <- data.frame(
    method = names(cms),
    aucc = vapply(cms, `[[`, numeric(1L), "aucc"),
    aucc_ratio = vapply(cms, `[[`, numeric(1L), "aucc_ratio"),
    stringsAsFactors = FALSE
  )
  
  merge(ci_df, aucc_df, by = "method", all.x = TRUE, sort = FALSE)
}




# Add Point Estimates and CI Skewness

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




# Run Bayesian Meta-Analysis

.run_bayesian_analysis <- function(estimates, SEs, meas, 
                                   tau_prior_scale_rr, tau_prior_scale_or,tau_prior_scale_smd, point_estimate = "median", sign_threshold = 0) {
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
  bm_significant <- !(sign_threshold >= bm_lower & sign_threshold <= bm_upper)
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




# Calculate Weighted Skewness

.calculate_weighted_skewness <- function(est, se) {
  w <- 1 / se^2
  theta_bar <- sum(w * est) / sum(w)
  num <- sum(w * (est - theta_bar)^3)
  den <- (sum(w * (est - theta_bar)^2)^(3/2)) / sqrt(sum(w))
  num / den
}