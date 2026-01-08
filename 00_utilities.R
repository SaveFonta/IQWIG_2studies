#######################################################
##          utilities.R -- Saverio Fontana       ######
#######################################################
library(metafor)
library(dplyr)
library(confMeta)



# ---- wrapper escalc that handles all cases ---- 
process_escalc <- function(df, zero_handling = "only0", ...) {
  
  # Ad output columns
  df <- df %>%
    mutate(
      yi = NA,  #effect size
      vi = NA,  #variance 
      sei = NA,  #se
      data_report = NA # IN which way do they report values? 
    )
  
  # Process row by row to handle each case individually
  for (i in 1:nrow(df)) {
    
    #Use trycatch just for fun
    tryCatch({
      
      #exctract type of Mass
      mass <- df$Mass[i]
      
      # ======================================================================
      #  BINARY OUTCOMES -> we have 2x2 Tables
      # ======================================================================
      
      # Check if 2x2 table is available
      
      if (!is.na(df$binaer_ai[i]) & !is.na(df$binaer_ni[i]) & 
          !is.na(df$binaer_ac[i]) & !is.na(df$binaer_nc[i])) {
        
        #Ecxtract:
        ai <- df$binaer_ai[i]
        bi <- df$binaer_ni[i] - df$binaer_ai[i]  # non-events in intervention
        ci <- df$binaer_ac[i]
        di <- df$binaer_nc[i] - df$binaer_ac[i]  # non-events in control
        n1i <- df$binaer_ni[i]
        n2i <- df$binaer_nc[i]
        
        
        #Here is the table they use
        
        # |  outcome 1	| outcome 2 |	total
        #group 1	|     ai	    !   bi	      !  n1i
        #group 2	|     ci	    !   di	      !  n2i        
        
        
        
        #NOTE: Escalc gives as output a df of escalc class w/ 2 cols: yi and vi
        
        if (mass == "OR") {
          # Odds Ratio
          res <- escalc(measure = "OR", 
                        ai = ai, bi = bi, ci = ci, di = di,
                        add = 0.5, to = zero_handling) #only0 is the default. Other options: (if0all)
          
          #add the values to the df
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "2x2_OR"
          
        } else if (mass == "RR") {
          # Risk Ratio
          res <- escalc(measure = "RR",
                        ai = ai, bi = bi, ci = ci, di = di,
                        add = 0.5, to = zero_handling)
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "2x2_RR"
          
        } else if (mass == "RD") {
          # Risk Difference
          res <- escalc(measure = "RD",
                        ai = ai, n1i = n1i, ci = ci, n2i = n2i)
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "2x2_RD"
        }
      }
      
      # ======================================================================
      # DIRECT EFFECT ESTIMATES -> (OR, RR, RD, HR with SE)
      # ======================================================================
      
      # we arrive here if there the column yi has not been filled from above, and if we have value in effekt_est and effekt_se
      
      if ( (is.na(df$yi[i])) & (!is.na(df$effekt_est[i])) & (!is.na(df$effekt_se[i]))) {    
        
        if (mass %in% c("OR", "OR (effekt)")) {
          # Already log OR
          res <- escalc(measure = "GEN",  #convert a regular data frame to an ‘escalc’ object for consistency!
                        yi = df$effekt_est[i], 
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "direct_OR"
          
        } else if (mass %in% c("RR", "RR (effekt)")) {
          # Already log RR
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "direct_RR"
          
        } else if (mass == "RD (effekt)") {
          # Risk difference 
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "direct_RD"
          
        } else if (mass == "HR") {
          # Hazard Ratio (already log HR)
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "HR"
          
        } else if (mass == "ROM") {
          # Ratio of Means (already log ROM)
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "ROM"
          
        } else if (mass == "IDR") {
          # Incidence Density Ratio (already log IDR)
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "IDR"
          
        } else if (mass == "SMD") {
          # easy SMD with SE
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "direct_SMD"
          
        } else if (mass == "MD") {
          #easy MD with SE
          res <- escalc(measure = "GEN",
                        yi = df$effekt_est[i],
                        sei = df$effekt_se[i])
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "direct_MD"
        }
      }
      
      # ======================================================================
      # CONTINUOUS OUTCOMES ->  MD with Pooled SD
      # ======================================================================
      
      # arrive here if we yet dont have an estimate yi, and if those cells with pooled stuff are populated
      
      if (is.na(df$yi[i]) & 
          !is.na(df$endpunkt_x1_md[i]) & !is.na(df$endpunkt_x1_spool[i]) &
          !is.na(df$endpunkt_x1_n_i[i]) & !is.na(df$endpunkt_x1_n_c[i])) {
        
        #Exctract
        md <- df$endpunkt_x1_md[i]
        sd_pool <- df$endpunkt_x1_spool[i]
        n1i <- df$endpunkt_x1_n_i[i]
        n2i <- df$endpunkt_x1_n_c[i]
        
        if (mass == "MD") {
          # Mean Difference with pooled SD
          # Use the MD measure by providing m1i, m2i (where m2i = 0 since we have the difference)
          # and both SDs as the pooled SD
          
          
          # Note that we need to do se(theta) = s_pooled (sqrt(1/ni + 1/ nc))
          #I checked inside escalc and that's exactly what it does, the fact that
          # we use m2i=0m but doesnt create any problems
          
          res <- escalc(measure = "MD",
                        m1i = md, m2i = 0, #
                        sd1i = sd_pool, sd2i = sd_pool, #vectors with the two sd of the two groups
                        n1i = n1i, n2i = n2i)
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "MD_pooled_SD"
          
        } else if (mass == "SMD") {
          # SMD = MD / pooled_SD
          # Use SMD measure with constructed means
          #empirically checked that using m2i = 0 doesnt change results
          
          res <- escalc(measure = "SMD",
                        m1i = md, m2i = 0,
                        sd1i = sd_pool, sd2i = sd_pool,
                        n1i = n1i, n2i = n2i)
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "SMD_from_pooled"
        }
      }
      
      # ======================================================================
      # CONTINUOUS OUTCOMES -> singular_studies_ (it means that we have the means and the se of each group)
      #    
      # ======================================================================
      
      # Yet we dont have an estimate, and those columns are populated
      if (is.na(df$yi[i]) &
          !is.na(df$stetig_mittel_i[i]) & !is.na(df$stetig_sd_i[i]) & 
          !is.na(df$stetig_n_i[i]) &
          !is.na(df$stetig_mittel_c[i]) & !is.na(df$stetig_sd_c[i]) & 
          !is.na(df$stetig_n_c[i])) {
        
        #Extract
        m1i <- df$stetig_mittel_i[i]
        sd1i <- df$stetig_sd_i[i]
        n1i <- df$stetig_n_i[i]
        m2i <- df$stetig_mittel_c[i]
        sd2i <- df$stetig_sd_c[i]
        n2i <- df$stetig_n_c[i]
        
        if (mass == "SMD") {
          # Standardized Mean Difference
          res <- escalc(measure = "SMD",
                        m1i = m1i, sd1i = sd1i, n1i = n1i,
                        m2i = m2i, sd2i = sd2i, n2i = n2i)
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "singular_studies_SMD"
          
        } else if (mass == "MD") {
          # Mean Difference
          res <- escalc(measure = "MD",
                        m1i = m1i, sd1i = sd1i, n1i = n1i,
                        m2i = m2i, sd2i = sd2i, n2i = n2i)
          df$yi[i] <- res$yi
          df$vi[i] <- res$vi
          df$sei[i] <- sqrt(res$vi)
          df$data_report[i] <- "singular_studies_MD"
        }
      }
      
    }, error = function(e) {
      warning(paste0("Error processing row", i, ":", e$message))
      df$data_report[i] <- "ERROR"
    })
  }
  
  
  
  out <- df %>% 
    mutate(
      logEst = yi,
      selogEst = sei,
      effect.measure = Mass,
      study = Studie,
      identifier = MA_id
    ) %>% 
    select(no, identifier, study, logEst, selogEst, data_report, effect.measure, sheet_name)
  
  
  
  message(
    paste0("Data processed with ", 
           sum(is.na(out$data_report)), 
           " values that were not suitable for computing \n")
  )
  return(out)
}






# ----- Formatting functions for graph and summaries ----


# Split up the MAs into groups according to the number of
# studies within them (easy to change)
group <- function(n_studies) {
  lvls <- c("2 studies", "3 studies", "> 3 studies")
  group1 <- function(n) {
    if (n == 2) {
      lvls[1]
    } else if (n == 3) {
      lvls[2]
    } else {
      lvls[3]
    }
  }
  res <- vapply(n_studies, group1, character(1L))
  res <- factor(res, levels = lvls) #trasform in factor
}

# Add group sizes to labels (for 2 studies, everyone will have same label)
add_group_size <- function(group_ind, strata) {
  stopifnot(is.factor(group_ind))
  stopifnot(is.atomic(strata))
  stopifnot(length(group_ind) == length(strata))
  counts <- stats::aggregate(
    x = strata ~ group_ind,
    FUN = function(x) length(unique(x))
  )
  cnt <- counts$strata
  names(cnt) <- counts$group_ind
  lvls <- levels(group_ind)
  new_lvls <- paste0(lvls, " (n = ", cnt[lvls], ")")
  levels(group_ind) <- new_lvls
  group_ind
}


extract_ma_metric <- function(cis_list, metric) {
  
  # Extract the specific metric (width/significant/whatever)
  res <- do.call("rbind", lapply(cis_list, function(x) x[[metric]]))
  
  #create a nice df specific for that metric
  res <- as.data.frame(res)
  
  # add number of studies 
  res$n_studies <- vapply(cis_list, function(x) nrow(x$inputs), integer(1L))
  
  #do the same thing by extracting measure from each element of cis_list and adding to a new column
  res$measure <- vapply(cis_list, function(x) x$measure, character(1L))
  
  
  # Format so that the MA name becomes a column
  res <- rownames_to_column(res, var = "ma")
  
  res <- pivot_longer(res, cols = !c(ma, n_studies, measure))
  res <- rename(res, "MA" = ma, "method" = name, !!metric := value) #nice! This is the dyplier way
  
  res <- mutate(res, n_studies = group(n_studies))
  res <- mutate(res, n_studies = add_group_size(group_ind = n_studies, strata = MA))
  
  return(res)
}





#########################################
#       Utilities for lemma checking    #
#########################################




# ----- Check conditions for a single meta-analysis -----
check_single_ma <- function(ma_result, alpha, edgington_method, fe_method) {
  
  # Extract study-level data
  inputs <- ma_result$inputs
  n_studies <- nrow(inputs)
  
  # Need 2 studies
  if (n_studies != 2) {
    return(list(
      applicable = FALSE,
      reason = "We need exactly 2 studies"
    ))
  }
  
  estimates <- inputs[, "estimate"]
  SEs <- inputs[, "SE"]
  
  # Extract widths for Edgington and Fixed Effect
  width_edg <- ma_result$width[edgington_method]
  width_fe <- ma_result$width[fe_method]
  
  # Check if methods exist
  if (is.na(width_edg) || is.na(width_fe)) {
    return(list(
      applicable = FALSE,
      reason = "Required methods not found in results"
    ))
  }
  
  # Calculate z-score for given alpha
  z_alpha <- qnorm(alpha/2)
  z_alpha_4 <- qnorm(sqrt(alpha/4))
  #!!
  
  # Extract variances
  sigma1 <- SEs[1]
  sigma2 <- SEs[2]
  sigma1_sq <- sigma1^2
  sigma2_sq <- sigma2^2
  
  # Extract estimates
  theta1_hat <- estimates[1]
  theta2_hat <- estimates[2]
  theta_diff <- abs(theta1_hat - theta2_hat)
  
  # ==============================================================================
  # Check Condition 1
  # ==============================================================================
  
  LHS1.1 <- max(sigma1, sigma2) / min(sigma1, sigma2) 
  
  RHS1.1 <- sqrt((z_alpha / z_alpha_4)^2 - 1)
  
  condition1_part1 <- LHS1.1 < RHS1.1  #ratio of se not too large
  
  LHS1.2 <- theta_diff
  RHS1.2 <- -z_alpha_4 * abs (sigma2 - sigma1)
  condition1_part2 <- LHS1.2 < RHS1.2
  
  condition1_holds <- condition1_part1 && condition1_part2 #estimates are close
  
  # ==============================================================================
  # Check Condition 2
  # ==============================================================================
  
  LHS2 <- -z_alpha_4 * abs (sigma2 - sigma1)
  CS2 <- theta_diff
  RHS2 <- (sigma1*sigma2) / sqrt(sigma1^2 + sigma2^2) * (-2 * z_alpha) + (sigma1 + sigma2) * z_alpha_4
  
  condition2_part1 <- LHS2 <= CS2 #estimate diff is at least somtng
  condition2_part2 <- CS2 < RHS2 #but still below sothing
  
  condition2_holds <- condition2_part1 && condition2_part2
  
  # ==============================================================================
  # Determine if Edgington is smaller
  # ==============================================================================
  
  edgington_smaller <- width_edg < width_fe
  
  # ==============================================================================
  # Classify the result
  # ==============================================================================
  
  if (condition1_holds && condition2_holds) {
    status <- "Both conditions hold"
  } else if (condition1_holds) {
    status <- "Only condition 1 holds"
  } else if (condition2_holds) {
    status <- "Only condition 2 holds"
  } else {
    status <- "Neither condition holds"
  }
  
  lemma_verified <- NA
  reverse_lemma_not_true <- NA
  
  #NOTE --> reverse_lemma_not_true mean that Edgington is smaller but neither conditions hold!
  if (edgington_smaller && (condition1_holds || condition2_holds)) {
    lemma_verified <- TRUE
  } else if (edgington_smaller && !condition1_holds && !condition2_holds) {
    reverse_lemma_not_true <- TRUE
  } else if (!edgington_smaller && (condition1_holds || condition2_holds)) {
    lemma_verified <- FALSE #this should never happen
  }
  
  # ==============================================================================
  # Return results
  # ==============================================================================
  
  list(
    applicable = TRUE,
    n_studies = n_studies,
    condition1_holds = condition1_holds,
    condition2_holds = condition2_holds,
    edgington_width = width_edg,
    fe_width = width_fe,
    edgington_smaller = edgington_smaller,
    width_difference = width_fe - width_edg,
    status = status,
    lemma_verified = lemma_verified,
    reverse_lemma_not_true = reverse_lemma_not_true
  )
}




# ----- Check conditions for a MANY meta-analysis -----

check_lemma_conditions <- function(ci_results,
                                   alpha = 0.05,
                                   edgington_method = "Edgington",
                                   fe_method = "Fixed effect") {
  
  # Initial condition, alpha < 1/4
  if (alpha>= 1/4) stop("alpha must be smaller than 1/4 to check the Lemma")
  

  
  # ==============================================================================
  
  
  #Apply to all meta-analyses
  
  lemma_checks <- lapply(ci_results, function(ma_result) {
    check_single_ma(ma_result, alpha, edgington_method, fe_method)
  })
  
  names(lemma_checks) <- names(ci_results)
  
  
  
  # Incorporat lemma checks to each ma_result
  
  for (i in seq_along(ci_results)) {
    ci_results[[i]]$lemma_check <- lemma_checks[[i]]
  }
  
  
  ci_results
}




extract_lemma_results <- function(ci_results_with_lemma) {
  
  lemma_list <- lapply(names(ci_results_with_lemma), function(ma_name) {
    result <- ci_results_with_lemma[[ma_name]]$lemma_check
    
    #if used for more than 2 studies
    if (!result$applicable) {
      return(data.frame(
        ma_name = ma_name,
        applicable = FALSE,
        reason = result$reason,
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      ma_name = ma_name,
      applicable = TRUE,
      n_studies = result$n_studies,
      condition1_holds = result$condition1_holds,
      condition2_holds = result$condition2_holds,
      edgington_smaller = result$edgington_smaller,
      edgington_width = result$edgington_width,
      fe_width = result$fe_width,
      width_difference = result$width_difference,
      status = result$status,
      lemma_verified = result$lemma_verified,
      reverse_lemma_not_true = result$reverse_lemma_not_true,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, lemma_list)
}




plot_lemma_results <- function(ci_results_with_lemma) {
  
  lemma_df <- extract_lemma_results(ci_results_with_lemma)
  lemma_df <- lemma_df[lemma_df$applicable, ]
  
  if (nrow(lemma_df) == 0) {
    stop("No applicable meta-analyses to plot")
  }
  
  # Create a categorical variable for plotting
  lemma_df$category <- with(lemma_df, {
    ifelse(condition1_holds & condition2_holds, "Both conditions",
           ifelse(condition1_holds, "Condition 1 only",
                  ifelse(condition2_holds, "Condition 2 only", "Neither condition")))
  })
  
  lemma_df$category <- factor(
    lemma_df$category,
    levels = c("Both conditions", "Condition 1 only", 
               "Condition 2 only", "Neither condition")
  )
  
  # Color by whether Edgington is actually smaller
  ggplot2::ggplot(lemma_df, 
                  ggplot2::aes(x = category, fill = edgington_smaller)) +
    ggplot2::geom_bar(position = "stack") +
    ggplot2::labs(
      title = "Lemma 4.3.1: Condition Holdings vs Actual Width Comparison",
      x = "Conditions from Lemma",
      y = "Number of Meta-Analyses",
      fill = "Edgington\nCI Smaller"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828"),
      labels = c("TRUE" = "Yes", "FALSE" = "No")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
