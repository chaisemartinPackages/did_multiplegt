printf <- function(...)print(sprintf(...))
fn_ctrl_rename <- function(x) { paste("ctrl", x, sep="_") }
fn_diff_rename <- function(x) { paste("diff", x, sep="_") }
fn_diff_i_rename <- function(x, i) { paste("diff", toString(i), "_", x, sep = "")}
fn_lag_rename <- function(x, k) { paste("L", toString(k), "_", x, sep="") }
fn_lead_rename <- function(x, k) { paste("F", toString(k), "_", x, sep="") }
get_controls_rename <- function(controls) { unlist(lapply(controls, fn_ctrl_rename)) }
get_diff_controls_rename <- function(controls) { unlist(lapply(controls, function(x) {fn_diff_rename(fn_ctrl_rename(x))})) }

generate_return_names <- function(dynamic = 0, placebo = 0) {
  names = c()

  if (placebo > 0) {
    for (i in placebo:1) {
      nm_effect_placebo = paste("placebo", toString(i), sep = "_")
      names = c(names, nm_effect_placebo)
    }
  }

  names = c(names, "effect")

  if (dynamic > 0) {
    for (i in 1:dynamic) {
      nm_effect_dynamic = paste("dynamic", toString(i), sep = "_")
      names = c(names, nm_effect_dynamic)
    }
  }
  names
}

generate_switchers_names <- function(dynamic = 0, placebo = 0) {
  names = c()

  if (placebo > 0) {
    for (i in placebo:1) {
      names = c(names, NA)
    }
  }

  nm_N_switchers = "N_switchers_effect"
  names = c(names, nm_N_switchers)

  if (dynamic > 0) {
    for (i in 1:dynamic) {
      nm_N_switchers_dynamic = paste("N", "switchers", "effect", toString(i), sep = "_")
      names = c(names, nm_N_switchers_dynamic)
    }
  }

  names
}

#' did_multiplegt_rename_var
#' @param df df
#' @param Y Y
#' @param G G
#' @param T T
#' @param D D
#' @param controls controls
#' @param recat_treatment recat_treatment
#' @param trends trends
#' @importFrom magrittr %>%
#' @import dplyr
#' @noRd
did_multiplegt_rename_var <- function(df, Y, G, T, D, controls, recat_treatment, trends) {
  controls_rename <- get_controls_rename(controls)

  original_names = c(Y, G, T, D, controls)
  new_names = c("Y", "G", "T", "D", controls_rename)

  df <- data.frame(df)

  if (!is.null(recat_treatment)) {
    original_names = c(original_names, recat_treatment)
    new_names = c(new_names, "Drecat")
  }

  if (!is.null(trends)) {
    df$Vtrends <- df[[trends]]
    original_names = c(original_names, "Vtrends")
    new_names = c(new_names, "Vtrends")
  }

  df <- df %>% select_at(vars(original_names))
  colnames(df) <- new_names
  df
}

did_multiplegt_transform <- function(df, controls, trends_nonparam) {
  # Group by time and group, calculate the counter
  df <- df %>%
    group_by(.data$G, .data$T) %>%
    mutate(counter = n()) %>%
    summarize_all(mean)

  # canonicalize time to start from 0 and increase consecutively
  tdf <- df %>% group_by(.data$T) %>% summarize(Tgroup = cur_group_id()) %>% select(.data$T, .data$Tgroup)
  df <- left_join(df, tdf, by="T") %>% mutate(T = .data$Tgroup) %>% select(-.data$Tgroup)

  # Mark all columns to NA if one is NA
  ignore_columns <- c("G", "T", "tag")
  if ("Vtrends" %in% colnames(df)) { ignore_columns <- c(ignore_columns, "Vtrends") }

  df <- df %>%
    mutate(tag = rowSums(across(.cols = !any_of("Y"), .fns = is.na))) %>%
    mutate_at(.vars = vars(-ignore_columns), .funs = list(~ ifelse(.data$tag, NA, .))) %>% select(-.data$tag)

  # canonicalize treatment to start from 0 and increase consecutively
  cat_treatment = if ("Drecat" %in% colnames(df)) { "Drecat" } else { "D" }
  var_cat_treatment = sym(cat_treatment)
  tdf <- df %>% group_by(!!var_cat_treatment) %>% summarize(Dgroup = cur_group_id()) %>% select(!!var_cat_treatment, .data$Dgroup)
  df <- left_join(df, tdf, by=cat_treatment)

  df <- df %>% mutate(DgroupLag = lag(.data$Dgroup))

  # calculate diff_Y, diff_D, diff_ctrl
  columns_to_diff = c("Y", "D", get_controls_rename(controls))
  ddf <- df %>%
    select_at(.vars = c("T", "G", columns_to_diff)) %>%
    group_by(.data$G) %>%
    mutate_at(.vars = columns_to_diff, .funs = function(x) { x - lag(x) }) %>%
    rename_at(.vars = columns_to_diff, .funs = fn_diff_rename )

  df <- left_join(df, ddf, by=c("T", "G"))

  # factorize T to Tfactor
  df$Tfactor <- factor(df$T)

  # process trends
  if ("Vtrends" %in% colnames(df)) {
    df$Vtrends <- factor(df$Vtrends)
  }

  # process trends_nonparam
  if (trends_nonparam) {
    tdf <- df %>% group_by(.data$Vtrends, .data$Tfactor) %>% summarize(VtrendsX = cur_group_id()) %>% select(.data$Vtrends, .data$Tfactor, .data$VtrendsX)
    tdf$VtrendsX <- factor(tdf$VtrendsX)

    df <- left_join(df, tdf, by=c("Vtrends", "Tfactor")) %>% mutate(Vtrends = .data$VtrendsX) %>% select(-.data$VtrendsX)
  }

  df
}

placebo_transform_level <- function(df, counter_placebo, controls) {
  nm_diff_controls = unlist(lapply(controls, function(x) {fn_diff_rename(fn_ctrl_rename(x))}))
  nm_lag_diff_D = fn_lag_rename("diff_D", counter_placebo)
  fn_lag_rename_i <- function(x) { fn_lag_rename(x, counter_placebo) }

  lag_names = c("diff_D", "diff_Y", nm_diff_controls)
  df_l <- df %>%
    group_by(.data$G) %>%
    select_at(.vars = vars(c("T", "G", lag_names))) %>%
    mutate_at(.vars = vars(lag_names), .funs = function(x) { lag(x, counter_placebo) }) %>%
    rename_at(.vars = vars(lag_names), .funs = fn_lag_rename_i)

  left_join(df, df_l, by=c("T", "G"))
}

dynamic_transform_level <- function(df, counter_dynamic, controls) {
  nm_ctrls = unlist(lapply(controls, fn_ctrl_rename))
  nm_diff_i_controls = unlist(lapply(nm_ctrls, function(x) {fn_diff_i_rename(x, counter_dynamic)}))
  nm_diff_i_Y = fn_diff_i_rename("Y", counter_dynamic)

  fn_lead_rename_i <- function(x) { fn_lead_rename(x, counter_dynamic) }

  lead_names = c("diff_D", nm_diff_i_Y, nm_diff_i_controls)

  df_l <- df %>%
    group_by(.data$G) %>%
    select_at(.vars = vars(c("T", "G", "Y", "diff_D", nm_ctrls))) %>%
    mutate_at(.vars = vars(c("Y", nm_ctrls)), .funs = function(x) { x - lag(x, counter_dynamic+1) }) %>%
    rename_at(.vars = vars(c("Y", nm_ctrls)), .funs = function(x) { fn_diff_i_rename(x, counter_dynamic) }) %>%
    mutate_at(.vars = vars(lead_names), .funs = function(x) { lead(x, counter_dynamic) }) %>%
    rename_at(.vars = vars(lead_names), .funs = fn_lead_rename_i)

  left_join(df, df_l, by=c("T", "G"))
}

did_multiplegt_preprocess <- function(df, Y, G, T, D, controls,
                                      placebo, dynamic,
                                      recat_treatment, trends_nonparam, trends_lin,
                                      cluster)
{
  cluster_column <- if (!is.null(cluster)) { df[[cluster]] } else { NULL }
  trends <- if (!is.null(trends_nonparam)) { trends_nonparam } else if (!is.null(trends_lin)) { trends_lin } else { NULL }

  df <- did_multiplegt_rename_var(df, Y, G, T, D, controls, recat_treatment, trends)
  df <- did_multiplegt_transform(df, controls, !is.null(trends_nonparam))

  if (placebo > 0) {
    for (counter_placebo in 1:placebo) {
      df <- placebo_transform_level(df, counter_placebo, controls)
    }
    df$placebo_cond = TRUE
  }

  if (dynamic > 0) {
    for (counter_dynamic in 1:dynamic) {
      df <- dynamic_transform_level(df, counter_dynamic, controls)
    }
    df$dynamic_cond = TRUE
  }

  if (!is.null(cluster)) { df$cluster <- cluster_column }
  df
}

placebo_update_cond <- function(df, counter_placebo, controls, threshold_stable_treatment) {
  var_lag_diff_D = sym(fn_lag_rename("diff_D", counter_placebo))
  df %>% mutate(placebo_cond = ifelse(!is.na(!!var_lag_diff_D) & abs(!!var_lag_diff_D) <= threshold_stable_treatment, .data$placebo_cond, FALSE))
}

#' prepare_placebo_once
#' @param df df
#' @param counter_placebo counter_placebo
#' @param controls controls
#' @param threshold_stable_treatment treshold_stable_treatment
#' @importFrom stringr str_remove
#' @noRd
prepare_placebo_once <- function(df, counter_placebo, controls, threshold_stable_treatment) {
  nm_diff_controls = unlist(lapply(controls, function(x) {fn_diff_rename(fn_ctrl_rename(x))}))
  prefix = paste("L", toString(counter_placebo), "_", sep="")

  nm_lag_diff_Y = fn_lag_rename("diff_Y", counter_placebo)
  nm_lag_diff_D = fn_lag_rename("diff_D", counter_placebo)
  nm_lag_diff_controls = unlist(lapply(nm_diff_controls, function(x) {
    fn_lag_rename(x, counter_placebo)
  }))

  df <- df %>%
    group_by(.data$G) %>%
    filter(n() > counter_placebo) %>%
    select_at(.vars = vars(-c("diff_Y", nm_diff_controls, nm_lag_diff_D))) %>%
    rename_at(.vars = vars(c(nm_lag_diff_Y, nm_lag_diff_controls)), .funs = list(~ str_remove(., prefix))) %>%
    filter(.data$placebo_cond == TRUE)

  df
}

dynamic_update_cond <- function(df, counter_dynamic, controls, threshold_stable_treatment) {
  var_lead_diff_D = sym(fn_lead_rename("diff_D", counter_dynamic))
  df %>% mutate(dynamic_cond = ifelse(!is.na(!!var_lead_diff_D) & abs(!!var_lead_diff_D) <= threshold_stable_treatment, .data$dynamic_cond, FALSE))
}

#' prepare_dynamic_once
#' @param df df
#' @param counter_dynamic counter_dynamic
#' @param controls controls
#' @param threshold_stable_treatment treshold_stable_treatment
#' @importFrom stringr str_replace
#' @noRd
prepare_dynamic_once <- function(df, counter_dynamic, controls, threshold_stable_treatment) {
  fn_lead_rename_i <- function(x) { fn_lead_rename(x, counter_dynamic) }

  nm_ctrls = unlist(lapply(controls, fn_ctrl_rename))
  nm_diff_controls = unlist(lapply(nm_ctrls, function(x) {fn_diff_rename(x)}))

  nm_diff_i_controls = unlist(lapply(nm_ctrls, function(x) {fn_diff_i_rename(x, counter_dynamic)}))
  nm_diff_i_Y = fn_diff_i_rename("Y", counter_dynamic)

  nm_lead_diff_i_Y = fn_lead_rename(nm_diff_i_Y, counter_dynamic)
  nm_lead_diff_D = fn_lead_rename("diff_D", counter_dynamic)
  nm_lead_diff_i_controls = unlist(lapply(nm_diff_i_controls, function(x) {
    fn_lead_rename(x, counter_dynamic)
  }))

  prefix = paste("F", toString(counter_dynamic), "_", "diff", toString(counter_dynamic), "_", sep="")

  df <- df %>%
    group_by(.data$G) %>%
    filter(n() > counter_dynamic) %>%
    select_at(.vars = vars(-c("diff_Y", nm_diff_controls))) %>%
    rename_at(.vars = vars(c(nm_lead_diff_i_Y, nm_lead_diff_i_controls)), .funs = list(~ str_replace(., prefix, "diff_"))) %>%
    filter(.data$dynamic_cond == TRUE)

  df
}
#' residuals_control_once
#' @param df df
#' @param d d
#' @param threshold_stable_treatment threshold_stable_treatment
#' @param diff_ctrl_names diff_ctrl_names
#' @param trends_nonparam trends_nonparam
#' @param trends_lin trends_lin
#' @importFrom stats as.formula predict lm
#' @importFrom fixest feols
#' @noRd
residuals_control_once <- function(df, d, threshold_stable_treatment, diff_ctrl_names, trends_nonparam, trends_lin) {
  df_d <- df %>% filter( .data$DgroupLag == d )
  df_stable <- df_d %>% filter( abs(.data$diff_D) <= threshold_stable_treatment ) %>% filter(!is.na(.data$diff_Y))
  n_stable <- nrow(df_stable)
  do_count_tfactor <- length(unique(df_stable$Tfactor)) > 1

  if (n_stable <= length(diff_ctrl_names)) {
    return(df_d %>% mutate(residuals = .data$diff_Y)  %>% select(.data$T, .data$G, .data$residuals))
  }

  df_trends <- NULL
  do_fixed_effect <- length(unique(df_stable$diff_Y)) > 1
  # Trends
  if (do_fixed_effect & ("Vtrends" %in% colnames(df_stable))) {
    do_count_vtrends <- length(unique(df_stable$Vtrends)) > 1
    formula = ""
    if (length(diff_ctrl_names) == 0) {
      formula = "diff_Y ~ 1"
    } else {
      formula = paste(diff_ctrl_names, collapse = " + ")
      formula = paste("diff_Y ~ ", formula, sep = "")
    }
    if (trends_lin & do_count_tfactor & do_count_vtrends) {
      formula = paste(formula, " | Vtrends + Tfactor", sep = "")
    } else if (do_count_vtrends) {
      formula = paste(formula, " | Vtrends", sep = "")
    }

    df_stable.lm <- feols(as.formula(formula), data = df_stable, weights = ~ counter)
    df_trends <- df_d %>% filter(.data$Vtrends %in% levels(df_stable$Vtrends))
    df_trends$pred_trends <- predict(df_stable.lm, df_trends)
    df_trends <- df_trends %>% select(.data$T, .data$G, .data$pred_trends)
  }

  # normal controls
  formula = paste(diff_ctrl_names, collapse = " + ")
  if (do_count_tfactor) formula = paste(formula, " + Tfactor")
  formula = paste("diff_Y ~ ", formula, sep = "")

  counter = df_stable$counter
  df_stable.lm <- lm(formula, data = df_stable, weights = counter)

  df_controls <-  if (do_count_tfactor) {
    df_d %>% filter(.data$Tfactor %in% df_stable.lm$xlevels[["Tfactor"]])
  } else { df_d }
  df_controls$pred_controls = predict(df_stable.lm, df_controls, allow.new.levels=TRUE)
  df_controls <- df_controls %>% select(.data$T, .data$G, .data$pred_controls)

  if (is.null(df_trends)) {
    df_d %>%
      left_join(df_controls, by=c("T", "G")) %>%
      mutate(residuals = case_when(!is.na(.data$pred_controls) ~ .data$diff_Y - .data$pred_controls,
                                   TRUE ~ .data$diff_Y)) %>%
      select(.data$T, .data$G, .data$residuals)
  } else {
    df_d %>%
      left_join(df_trends, by=c("T", "G")) %>%
      left_join(df_controls, by=c("T", "G")) %>%
      mutate(residuals = case_when(!is.na(.data$pred_trends) ~ .data$diff_Y - .data$pred_trends,
                              !is.na(.data$pred_controls) ~ .data$diff_Y - .data$pred_controls,
                              TRUE ~ .data$diff_Y)) %>%
      select(.data$T, .data$G, .data$residuals)
  }
}

#' residuals_control
#' @param df df 
#' @param controls controls
#' @param threshold_stable_treatment threshold_stable_treatment
#' @param trends_nonparam trends_nonparam
#' @param trends_lin trends_lin
#' @importFrom stats residuals
#' @noRd
residuals_control <- function(df, controls, threshold_stable_treatment, trends_nonparam, trends_lin) {
  if (length(controls) == 0 & !trends_nonparam & !trends_lin) {
    return(df)
  }

  diff_ctrl_names <- unlist(lapply(
    controls, function(x) { fn_diff_rename(fn_ctrl_rename(x)) } ))

  d_min <- min(df$DgroupLag, na.rm = TRUE)
  d_max <- max(df$DgroupLag, na.rm = TRUE)

  for (d in d_min:d_max) {
    tmp_ret <- residuals_control_once(
      df, d, threshold_stable_treatment, diff_ctrl_names, trends_nonparam, trends_lin)
    df <- df %>%
      left_join(tmp_ret, by = c("T", "G")) %>%
      mutate(diff_Y = ifelse(.data$DgroupLag == d, residuals, .data$diff_Y)) %>%
      select(-residuals)
  }

  df
}

#' did_multiplegt_once
#' @param df df
#' @param d d 
#' @param threshold_stable_treatment threshold_stable_treatment
#' @importFrom stats lm
#' @noRd
did_multiplegt_once <- function(df, d, threshold_stable_treatment) {
  ret = list(effect = 0, denom = 0, total = 0, switchers = 0)
  tmp_cnt_df <- df %>% filter(.data$DgroupLag == d)

  df_increase <- tmp_cnt_df %>% filter(.data$diff_D > threshold_stable_treatment)
  n_increase <- as.numeric(nrow(df_increase))
  cnt_increase <- as.numeric(sum(df_increase$counter, na.rm = TRUE))

  df_stable <- tmp_cnt_df %>% filter( abs(.data$diff_D) <= threshold_stable_treatment )
  n_stable <- as.numeric(nrow(df_stable))
  cnt_stable <- as.numeric(sum(df_stable$counter, na.rm = TRUE))

  df_decrease <- tmp_cnt_df %>% filter(.data$diff_D < -threshold_stable_treatment)
  n_decrease <- as.numeric(nrow(df_decrease))
  cnt_decrease <- as.numeric(sum(df_decrease$counter, na.rm = TRUE))

  if (n_increase * n_stable > 0) {
    ret$total = ret$total + cnt_increase + cnt_stable
    ret$switchers = ret$switchers + cnt_increase

    df_plus <- rbind(df_increase, df_stable)
    df_plus$treatment <- case_when(df_plus$diff_D > threshold_stable_treatment ~ 1, TRUE ~ 0)

    counter = df_plus$counter
    
    df_plus.lm <- lm(diff_Y ~ treatment, data = df_plus, weights = counter)
    ret$effect = ret$effect + df_plus.lm$coefficients["treatment"] * cnt_increase

    df_plus.lm <- lm(diff_D ~ treatment, data = df_plus, weights = counter)
    ret$denom = ret$denom + df_plus.lm$coefficients["treatment"] * cnt_increase
  }

  if (n_decrease * n_stable > 0) {
    ret$total = ret$total + cnt_decrease + cnt_stable
    ret$switchers = ret$switchers + cnt_decrease

    df_minus <- rbind(df_decrease, df_stable)
    df_minus$treatment <- case_when(df_minus$diff_D < -threshold_stable_treatment ~ 1, TRUE ~ 0)

    counter = df_minus$counter
    
    df_minus.lm <- lm(diff_Y ~ treatment, data = df_minus, weights = counter)
    ret$effect = ret$effect - df_minus.lm$coefficients["treatment"] * cnt_decrease

    df_minus.lm <- lm(diff_D ~ treatment, data = df_minus, weights = counter)
    ret$denom = ret$denom - df_minus.lm$coefficients["treatment"] * cnt_decrease
  }

  if (n_increase * n_decrease * n_stable > 0) {
    ret$total = ret$total - cnt_stable
  }

  ret
}


did_multiplegt_core <- function(df, controls, threshold_stable_treatment, trends_nonparam, trends_lin,
                                counter_placebo = 0, counter_dynamic = 0, t_max = 0) {
  effect = 0
  denom = 0
  total = 0
  switchers = 0

  df <- residuals_control(df, controls, threshold_stable_treatment, trends_nonparam, trends_lin)

  for (t in (counter_placebo+2):(t_max - counter_dynamic)) {
    df_d <- df %>% filter(.data$T == t) %>% filter(!is.na(.data$diff_D) & !is.na(.data$diff_Y))
    if (nrow(df_d) > 0) {
      d_min = min(df_d$DgroupLag, na.rm = TRUE)
      d_max = max(df_d$DgroupLag, na.rm = TRUE)
      for (d in d_min:d_max) {
        tmp_ret = did_multiplegt_once(df_d, d, threshold_stable_treatment)
        effect = effect + tmp_ret$effect
        denom = denom + tmp_ret$denom
        total = total + tmp_ret$total
        switchers = switchers + tmp_ret$switchers
      }
    }
  }

  list(effect = effect / denom, N_effect = total, N_switchers_effect = switchers)
}

did_multiplegt_estim <- function(df, controls, placebo, dynamic, threshold_stable_treatment, trends_nonparam, trends_lin) {
  t_max <- max(df$T)
  ret <- did_multiplegt_core(df, controls, threshold_stable_treatment,
                             trends_nonparam, trends_lin,
                             t_max=t_max)

  # dynamic
  if (dynamic > 0) {
    df_dynamic_preprocessed = df

    for (i in 1:dynamic) {
      nm_effect_dynamic = paste("dynamic", toString(i), sep = "_")
      nm_N_dynamic = paste("N", "dynamic", toString(i), sep = "_")
      nm_N_switchers_dynamic = paste("N", "switchers", "effect", toString(i), sep = "_")

      df_dynamic_preprocessed <- dynamic_update_cond(df_dynamic_preprocessed, i, controls, threshold_stable_treatment)
      df_dynamic_lead <- prepare_dynamic_once(df_dynamic_preprocessed, i, controls,
                                              threshold_stable_treatment)
      if (nrow(df_dynamic_lead) == 0) {
        ret[nm_N_dynamic] <- NA
        next
      }

      ret_dynamic_lag <- did_multiplegt_core(df_dynamic_lead, controls, threshold_stable_treatment,
                                             trends_nonparam, trends_lin,
                                             counter_dynamic = i,
                                             t_max = t_max)

      ret[nm_effect_dynamic] <- ret_dynamic_lag$effect
      ret[nm_N_dynamic] <- ret_dynamic_lag$N_effect
      ret[nm_N_switchers_dynamic] <- ret_dynamic_lag$N_switchers_effect
    }
  }

  # placebo effect
  if (placebo > 0) {
    df_placebo_preprocessed = df
    for (i in 1:placebo) {
      nm_effect_placebo = paste("placebo", toString(i), sep = "_")
      nm_N_placebo = paste("N", "placebo", toString(i), sep = "_")

      df_placebo_preprocessed <- placebo_update_cond(df_placebo_preprocessed, i, controls, threshold_stable_treatment)
      df_placebo_lag <- prepare_placebo_once(df_placebo_preprocessed, i, controls,
                                             threshold_stable_treatment)
      if (nrow(df_placebo_lag) == 0) {
        ret[nm_N_placebo] <- NA
        next
      }

      ret_placebo_lag <- did_multiplegt_core(df_placebo_lag, controls, threshold_stable_treatment,
                                             trends_nonparam, trends_lin,
                                             counter_placebo = i,
                                             t_max = t_max)

      ret[nm_effect_placebo] <- ret_placebo_lag$effect
      ret[nm_N_placebo] <- ret_placebo_lag$N_effect
    }
  }

  ret
}

#' did_multiplegt_bootstrap
#' @param df df
#' @param controls controls
#' @param brep brep
#' @param trends_nonparam trends_nonparam
#' @param trends_lin trends_lin
#' @param has_cluster has_cluster
#' @param placebo placebo
#' @param dynamic dynamic
#' @param threshold_stable_treatment threshold_stable_treatment
#' @param parallel parallel
#' @importFrom parallel detectCores mclapply
#' @importFrom sampling getdata cluster srswr
#' @noRd
did_multiplegt_bootstrap = function(df, controls, brep, trends_nonparam, trends_lin, has_cluster = FALSE,
                                    placebo = 0, dynamic = 0, threshold_stable_treatment = 0,
                                    parallel = FALSE) {
  names = generate_return_names(dynamic, placebo)
  N = nrow(df)
  K = 1 + placebo + dynamic
  N_cluster = if (has_cluster) { length(unique(df$cluster)) } else { 0 }

  callback = function(i) {
    samples = if (has_cluster) {
      cs = sampling::cluster(df, "cluster", N_cluster, method = "srswr")
      cs1 = rep(0, N)
      cs1[cs$ID_unit] = cs$Replicates
      getdata(df, cs1)
    } else {
      getdata(df, srswr(N, N))
    }

    ret = c()
    rtmp = did_multiplegt_estim(samples, controls, placebo, dynamic, threshold_stable_treatment, trends_nonparam, trends_lin)
    for (name in names) {
      ret = c(ret, unlist(rtmp[name]))
    }
    ret
  }

  if (parallel) {
    numCores <- detectCores()
    matrix(unlist(mclapply(1:brep, callback, mc.cores = numCores)), nrow = brep, ncol = K, byrow = TRUE)
  } else {
    matrix(unlist(lapply(1:brep, callback)), nrow = brep, ncol = K, byrow = TRUE)
  }
}

#' plot_bootstrap_results
#' @param placebo placebo
#' @param dynamic dynamic
#' @param X X
#' @param LB LB
#' @param UB UB
#' @importFrom graphics axis lines
#' @importFrom plotrix plotCI
#' @noRd
plot_bootstrap_result <- function(placebo, dynamic, X, LB, UB) {
  points = seq(-placebo, dynamic)
  plotCI(points, X, ui=UB, li=LB, col="navyblue",scol="firebrick4", pch = 19, cex=0.5,
         xlab = "Time since treatment", ylab= "Treatment effect", xaxt = "n")
  lines(points, X, col = 'navyblue')
  axis(side=1, at = points)
}

#' did_multiplegt_old
#' @importFrom stats sd cov
#' @md 
#' @description Estimates the effect of a treatment on an outcome, in sharp DID designs with multiple groups and periods.
#' @param df the data frame for input
#' @param Y the name of Y variable
#' @param G the name of group variable
#' @param T the name of time variable
#' @param D the name of treatment variable
#' @param controls the list of names of control variables, empty if not specified
#' @param placebo the number of placebo estimators to be estimated. Placebo estimators compare switchers' and non-switchers' outcome evolution before switchers' treatment changes. Under the parallel trends assumption underlying the \eqn{DID_M} estimator, the placebo estimators should not significantly differ from 0. The number of placebos requested can be at most equal to the number of time periods in the data minus 2.
#' @param dynamic the number of dynamic treatment effects to be estimated. This option should only be used in staggered adoption designs, where each group's treatment is weakly increasing over time, and when treatment is binary. The estimators of dynamic effects are similar to the \eqn{DID_M} estimator, except that they make use of long differences of the outcome (e.g. from \eqn{t-1} to \eqn{t+1}) rather than first differences. The number of dynamic effects requested can be at most equal to the number of time periods in the data minus 2.
#' @param threshold_stable_treatment this option may be useful when the treatment is continuous, or takes a large number of values. The DIDM estimator uses as controls groups whose treatment does not change between consecutive time periods.  With a continuous treatment, there may not be any pair of consecutive time periods between which the treatment of at least one group remains perfectly stable. For instance, if the treatment is rainfall and one uses a county \eqn{\times} year data set, there is probably not a single county*year whose rainfall is exactly the same as in the same county in the previous year.  Then, one needs to specify the \code{threshold_stable_treatment = #} option, with # a positive real number.  For each pair of consecutive time periods, the command will use counties whose rainfall changed in absolute value by less than # as controls. # should be large enough so that there are counties whose rainfall levels change by less than # between two consecutive years, but it should be small enough so that a change in rainfall of # would be unlikely to affect the outcome.
#' @param recat_treatment pools some values of the treatment together when determining the groups whose outcome evolution are compared. This option may be useful when the treatment takes a large number of values, and some very rare in the sample. For instance, assume that treatment D takes the values 0, 1, 2, 3, and 4, but few observations have a treatment equal to 2. Then, there may be a pair of consecutive time periods where one group goes from 2 to 3 units of treatment, but no group has a treatment equal to 2 at both dates. To avoid loosing that observation, one can create a variable \code{D_recat} that takes the same value when D=1 or 2 (e.g.: \code{D_recat=(D>=1)+(D>=3)+(D>=4)}), and then specify the \code{recat_treatment = "D_recat"} option. Then, the command can also use groups with a treatment equal to 1 at two consecutive dates as controls for groups going from 2 to 3 units of treatment, thus making it more likely that all switchers have a non-empty set of controls.
#' @param trends_nonparam when this option is specified, time fixed effects interacted with varlist are included in the estimation. varlist can only include one categorical variable. For instance, if one works with county \eqn{\times} year data set and one wants to allow for state-specific trends, then one should write \code{trends_nonparam = "state"}, where state is the state identifier.
#' @param trends_lin when this option is specified, linear time trends interacted with varlist are included in the estimation. varlist can only include one categorical variable. For instance, if one works with a year data set and one wants to allow for village-specific linear trends, one should write \code{trends_lin = "village"}, where village is the village identifier. The \code{trends_nonparam= varlist} and \code{trends_lin = varlist} cannot be specified at the same time.
#' @param brep The number of bootstrap replications to be used in the computation of estimators' standard errors. If the option is specified, \code{did_multiplegt_old} returns a graph with all the estimated treatment effects and placebos, and their 95 % confidence intervals constructed using a normal approximation. Otherwise, the command does not compute estimators' standard errors. If the option is specified, it plots a graph with all the estimated treatment effects and placebos, and their 95 % confidence intervals constructed using a normal approximation.
#' @param cluster the standard errors of the estimators using a block bootstrap at the varname level. Only one clustering variable is allowed.
#' @param covariance if this option and the \code{brep = #} option are specified, the command computes the covariances between all the pairs of instantaneous and dynamic effects requested, and between all the pairs of placebos requested. This option can be useful to assess whether some average of the instantaneous and dynamic effects is statistically significant. For instance, assume that you estimate the instantaneous effect, effect_0, and one dynamic effect, effect_1. You would assess whether \eqn{2/3} effect_0+ \eqn{1/3} effect_1, a weighted average of those two effects, is statistically significant. You can specify the covariances option, use the fact that Var(\eqn{2/3} effect_0+\eqn{1/3} effect_1)=4/9V(effect_0)+1/9V(effect_1)+4/9cov(effect_0,effect_1) to compute the standard error of \eqn{2/3} effect_0+\eqn{1/3} effect_1, and finally assess if this average effect is significant. This option can also be useful to run an F-test of whether the placebos are all equal to 0, when several placebos are requested.
#' @param average_effect if that option is specified, the command will compute an average of the instantaneous and dynamic effects requested. If \code{average_effect = "simple"} is specified, the command will compute ple average of the effects and its standard error. If \code{average_effect = "prop_number_switchers"} is specified, the command will compute an average where each effect receives a weight proportional to the number of switchers the effect to. When average_effect is specified, the covariances option also has to be specified, and the number of dynamic effects requested should be greater than or equal to 1. 
#' @param parallel perform bootstrap on multicore if \code{TRUE}.
#' 
#' @section Overview:
#' 
#' did_multiplegt_old estimates the effect of a treatment on an outcome, using group- (e.g. county- or state-) level panel data with multiple groups and periods.  Like other recently proposed DID estimation commands (did, didimputation...), did_multiplegt can be used with a binary and staggered (absorbing) treatment. But unlike those other commands, did_multiplegt_old can also be used with a non-binary treatment (discrete or continuous) that can increase or decrease multiple times. The panel of groups may be unbalanced: not all groups have to be observed at every period (see FAQ section for more info on that). The data may also be at a more disaggregated level than the group level (e.g. individual-level wage data to measure the effect of a regional-level minimum-wage on individuals' wages).
#'
#' It computes the \eqn{DID_M} estimator introduced in Section 4 of Chaisemartin and D'Haultfoeuille (2019), which generalizes the standard DID estimator with two groups, two periods and a binary treatment to situations with many groups,many periods and a potentially non-binary treatment. For each pair of consecutive time periods \eqn{t-1} and \eqn{t} and for each value of the treatment \eqn{d}, the package computes a \eqn{DID} estimator comparing the outcome evolution among the switchers, the groups whose treatment changes from \eqn{d} to some other value between \eqn{t-1} and \eqn{t}, to the same evolution among control groups whose treatment is equal to \eqn{d} both in \eqn{t-1} and \eqn{t}. Then the \eqn{DID_M} estimator is equal to the average of those \eqn{DID}s across all pairs of consecutive time periods and across all values of the treatment. Under a parallel trends assumption, \eqn{DID_M} is an unbiased and consistent estimator of the average treatment effect among switchers, at the time period when they switch.
#'
#' The package can also compute placebo estimators that can be used to test the parallel trends assumption.
#'
#' Finally, in staggered adoption designs where each group's treatment is weakly increasing over time, it can compute estimators of switchers' dynamic treatment effects, one time period or more after they have started receiving the treatment. 
#' 
#' WARNING: To estimate event-study/dynamic effects, we strongly recommend using the much faster did_multiplegt_dyn command, available from the CRAN repository. In addition to that, did_multiplegt_dyn offers more options than did_multiplegt_old.
#' 
#' @examples
#' # estimating the effect of union membership on wages
#' # using the same panel of workers as in Vella and Verbeek (1998)
#' data("wagepan_mgt")
#' Y = "lwage"
#' G = "nr"
#' T = "year"
#' D = "union"
#' controls = c("hours")
#'
#' did_multiplegt_old(wagepan_mgt, Y, G, T, D, controls)
#' @return \code{did_multiplegt_old} returns an object class that has the following values
#'         effect, effect of the treatment
#'         se_effect, standard error of the treatment when bootstraping
#'         N_effect, number of samples used
#'         placebo_i, estimated placebo effect i periods before switchers switch treatment, for all i in 0, 1, ..., k
#'         se_placebo_i, estimated standard error of placebo_i, if the option brep has been specified
#'         N_placebo_i, number of observations used in the estimation of placebo_i
#'         placebo_i, estimated dynamic effect i periods, for all i in 0, 1, ..., k
#'         se_placebo_i, estimated standard error of dynamic_i, if the option brep has been specified
#'         N_placebo_i, number of observations used in the estimation of dynamic_i
#' @export

did_multiplegt_old <- function(df, Y, G, T, D, controls = c(),
                           placebo = 0, dynamic = 0,
                           threshold_stable_treatment = 0,
                           recat_treatment = NULL,
                           trends_nonparam = NULL, trends_lin = NULL,
                           brep = 0, cluster = NULL,
                           covariance = FALSE,
                           average_effect = NULL,
                           parallel = FALSE)
{
  df_preprocess <- did_multiplegt_preprocess(df, Y, G, T, D, controls,
                                             placebo, dynamic,
                                             recat_treatment,
                                             trends_nonparam, trends_lin,
                                             cluster)

  trends_nonparam = !is.null(trends_nonparam)
  trends_lin = !is.null(trends_lin)
  eret = did_multiplegt_estim(df_preprocess, controls, placebo, dynamic, threshold_stable_treatment,
                              trends_nonparam, trends_lin)

  if (brep == 0) {
    eret
  } else {
    bret = did_multiplegt_bootstrap(df_preprocess, controls, brep, trends_nonparam, trends_lin,
                                    has_cluster = !is.null(cluster),
                                    placebo = placebo, dynamic = dynamic,
                                    threshold_stable_treatment = threshold_stable_treatment,
                                    parallel = parallel)

    names = generate_return_names(dynamic, placebo)
    switcher_names = generate_switchers_names(dynamic, placebo)
    ret = NULL
    X = c()
    LB = c()
    UB = c()
    for (i in 1:length(names)) {
      name = names[i]
      Nname = paste("N_", name, sep="")
      se_name = paste("se_", name, sep="")

      x = as.numeric(eret[name])
      s = as.numeric(sd(bret[,i]))
      N = eret[Nname]

      ret[name] = x
      ret[se_name] = s
      ret[Nname] = N

      switcher_name = switcher_names[i]
      if (!is.na(switcher_name)) {
        ret[switcher_name] = eret[switcher_name]
      }

      X = c(X, x)
      LB = c(LB, x - s*1.96)
      UB = c(UB, x + s*1.96)
    }

    if (covariance) {
      base = placebo + 1
      # placebo
      for (i in 1:placebo) {
        if (i >= placebo) {
          break
        }
        for (j in (i+1):placebo) {
          r = cov(bret[,base-i], bret[,base-j])
          name = paste("cov_placebo", toString(i), toString(j), sep="_")
          ret[name] = r
        }
      }
      # dynamic
      for (i in 0:dynamic) {
        if (i >= dynamic) { break }
        for (j in (i+1):dynamic) {
          r = cov(bret[,i+base], bret[,j+base])
          name = paste("cov_effect", toString(i), toString(j), sep="_")
          ret[name] = r
        }
      }

      if (!is.null(average_effect)) {
        weight = rep(0, dynamic + 1)
        start = placebo+1
        end = placebo + 1 + dynamic

        if (average_effect == "simple") {
          weight = rep(1.0 / (dynamic + 1), dynamic + 1)
        } else if (average_effect == "prop_number_switchers") {
          weight = unlist(eret[switcher_names[start:end]])
          sum_weight = sum(weight)
          weight = weight / sum_weight
        }

        effect_average = sum(unlist(eret[names[start:end]]) * weight)
        N_effect_vec = unlist(eret[mapply(function(x) {paste("N", x, sep="_")}, names[start:end])])
        N_effect_average = sum(N_effect_vec)
        var_effect_vec = unlist(ret[mapply(function(x) {paste("se", x, sep="_")}, names[start:end])])
        var_effect_average = sum((weight * var_effect_vec)^2)
        for (i in 1:dynamic) {
          if (i >= dynamic) { break }
          for (j in (i+1):dynamic) {
            name = paste("cov_effect", toString(i), toString(j), sep="_")
            var_effect_average = var_effect_average + weight[i+1] * weight[j+1]*2*unlist(ret[name])
          }
        }

        ret["effect_average"] = effect_average
        ret["N_effect_average"] = N_effect_average
        ret["se_effect_average"] = sqrt(var_effect_average)

        for (i in 0:dynamic) {
          name = paste("weight_effect_", toString(i), sep="")
          ret[name] = weight[i+1]
        }
      }
    }

    plot_bootstrap_result(placebo, dynamic, X, LB, UB)
    ret

    message("To estimate event-study/dynamic effects, we strongly recommend using the much faster did_multiplegt_dyn command, available from the CRAN repository. In addition to that, did_multiplegt_dyn offers more options than did_multiplegt.")

  }
}
