# *******************************************************************************
# CORE FUNCTIONS
# *******************************************************************************

# -------------------------------------------------------------------------------
# Create Filtered Sensor Dataframes by category
# -------------------------------------------------------------------------------

#' Extract specified column from the erp data and filter by the
#' specified category name
#'
#' @param df tibble. The specified
#' @param colname
#' @param cat_idx
#'
#' @return
#' @export
extract_column <- function(df, colname) {
  df %>%
    dplyr::select(colname) %>%
    base::return()
}

#' Extract specified column from the erp data and filter by the
#' specified category name
#'
#' @param df tibble. The specified
#' @param colname
#' @param cat_idx
#'
#' @return
#' @export
filter_rows <- function(df, cat_idx) {
  df %>%
    dplyr::filter(lab_category == cat_idx) %>%
    base::return()
}

#' Title
#'
#' @param inp_erp_df_lab
#' @param cat_idx
#'
#' @return
#' @export
#'
#' @examples
erp_create_filter_df <- function(inp_erp_df_lab, cat_idx = NULL) {
  if (is.null(cat_idx)) {
    filt_df <- inp_erp_df_lab
  } else {
    filt_df <- purrr::map(
      .x = inp_erp_df_lab
      , ~ filter_rows(
        df = .x
        , cat_idx = cat_idx
      )
    )
  }
  base::return(filt_df)
}

# -------------------------------------------------------------------------------
# Create COVARIANCE Matrices (by category) - UNSMOOTHED
# -------------------------------------------------------------------------------

#' Title
#'
#' @param inp_erp_filt_df_lab
#' @param time_int
#' @param cat_idx
#'
#' @return
#' @export
create_covmats <- function(inp_erp_filt_df_lab, time_int = NULL
                           , cat_idx = NULL) {
  covmat_list <- base::list()

  if (is.null(time_int)) {
    # TODO: remove the hardcoded 6 label cols here
    time_int <- 1:(ncol(inp_erp_filt_df_lab[[1]]) - 6)
  }

  for (i in time_int) {

    # Set up time based values
    time_val <- i
    print(time_val)
    col_val <- stringr::str_c("X", time_val, sep = "")
    print(col_val)

    # Create the correlation matrix
    # Create the labels for the channels
    channel_labs <- stringr::str_c(
      "C", seq(
        from = 1
        , to = length(inp_erp_filt_df_lab)
      )
      , sep = ""
    )

    # Extract the X_i column
    corr_base <- purrr::map(
      .x = inp_erp_filt_df_lab
      , ~ extract_column(
        df = .x
        , colname = col_val
      )
    ) %>%
      purrr::map_dfc(., cbind) %>%
      magrittr::set_colnames(channel_labs)

    # Create the Correlation matrix
    # Source: https://stackoverflow.com/questions/47475897/correlation-matrix-tidyr-gather-v-reshape2-melt
    covmat <- corr_base %>%
      cov(x = ., method = "pearson")

    covmat_list[[length(covmat_list) + 1]] <- covmat
  }
  base::return(covmat_list)
}

# -------------------------------------------------------------------------------
# Create Covariance Matrices - SMOOTHED
# -------------------------------------------------------------------------------

# Kernel smoothing helper function
#' Title
#'
#' @param i
#' @param j
#' @param mat
#'
#' @return
#' @export
extract_vec <- function(i, j, mat) {
  base::return(mat[i, j])
}

# Kernel smoothing function - low bandwidth here to have really smoothed
# values
#' Title
#'
#' @param i
#' @param j
#' @param inp_mat
#' @param bwth
#'
#' @return
#' @export
smooth_vec <- function(i, j, inp_mat, bwth = 0.5) {
  Y <- purrr::map_dbl(.x = inp_mat, ~ extract_vec(i = i, j = j, mat = .x))
  X <- 1:length(inp_mat)
  out_smooth <- ksmooth(
    x = X, y = Y, kernel = c("normal")
    , bandwidth = bwth
    , x.points = X
  )$y
  base::return(out_smooth)
}

#' Title
#'
#' @param inp_mat
#'
#' @return
#' @export
#'
#' @examples
erp_def_rowcolnames_mat <- function(inp_mat) {
  # Create the correlation matrix
  # Create the labels for the channels
  channel_labs <- stringr::str_c(
    "C", seq(
      from = 1
      , to = base::nrow(inp_mat)
    )
    , sep = ""
  )

  base::rownames(inp_mat) <- channel_labs
  base::colnames(inp_mat) <- channel_labs

  base::return(inp_mat)
}

#' Title
#'
#' @param mat_list
#' @param bwth
#'
#' @return
#' @export
erp_ksmooth_matrix_list <- function(mat_list, bwth) {
  # Get the number of rows for a sample covariance matrix
  # Must be a square matrix!
  nsize <- nrow(mat_list[[1]])

  # Perform the kernel smoothing for every corresponding
  # matrix entry
  smooth_vec_list <- tidyr::crossing(i = 1:nsize, j = 1:nsize) %>%
    purrr::pmap(
      .l = .
      , ~ smooth_vec(
        i = .x
        , j = .y
        , inp_mat = mat_list
        , bwth = bwth
      )
    )

  # Put the smoothed vectors for each matrix entry back
  # into the original form
  smooth_mat <- smooth_vec_list %>%
    base::do.call(rbind, .) %>%
    base::data.frame() %>%
    purrr::map(~ matrix(
      data = .
      , nrow = nsize
      , ncol = nsize
      , byrow = TRUE
    )) %>%
    base::unname()

  smooth_mat <- purrr::map(
    .x = smooth_mat
    , ~ erp_def_rowcolnames_mat(inp_mat = .x)
  )


  base::return(smooth_mat)
}

# -------------------------------------------------------------------------------
# Create CORRELATION Matrices - UNSMOOTHED
# -------------------------------------------------------------------------------

#' Title
#'
#' @param inp_covmat
#' @param abs
#' @param fisher
#'
#' @return
#' @export
erp_cov2corr <- function(inp_covmat, abs = TRUE, fisher = TRUE) {
  cormat <- stats::cov2cor(V = inp_covmat) %>%
    base::round(x = ., digits = 10)
  if (abs) {
    # Get the absolute values of the correlation matrices
    cormat <- abs(cormat)
  }

  if (fisher) {
    # Apply the fisher transformation
    # https://en.wikipedia.org/wiki/Fisher_transformation
    cormat <- base::atanh(cormat)
  }

  base::return(cormat)
}

# -------------------------------------------------------------------------------
# Diff to Pre-stim
# TODO: Check if this is smoothed or unsmoothed
# -------------------------------------------------------------------------------

#' Title
#'
#' @param src_mat
#' @param comp_mat
#'
#' @return
#' @export
get_mat_diff <- function(src_mat, comp_mat) {
  # Take the differences of the comparison matrix to the src
  # baseline matrix
  base::return(comp_mat - src_mat)
}

#' Title
#'
#' @param cormats
#' @param n
#'
#' @return
#' @export
erp_avg_firstn_cormats <- function(cormats, n) {
  corr_fn <- cormats[1:n]
  Y <- do.call(cbind, corr_fn)
  Y <- array(Y, dim = c(dim(corr_fn[[1]]), length(corr_fn)))
  avg_cormats <- apply(Y, c(1, 2), mean, na.rm = TRUE)
  base::return(avg_cormats)
}

#' Title
#'
#' @param baseline_src_mat
#' @param cormats
#' @param avg_n
#'
#' @return
#' @export
create_avg_cormats <- function(baseline_src_mat = NULL, cormats, avg_n) {

  # We will take differences to baseline for all correlation matrices
  rem_n <- 1:length(cormats)

  # Get the List of the remaining correlation matrices after the first n
  corr_rem <- cormats[rem_n]

  if (is.null(baseline_src_mat)) {
    # Get the first n corelation matrices
    avg_cormats <- erp_avg_firstn_cormats(
      cormats = cormats
      , n = avg_n
    )
  } else {
    # Get the first n corelation matrices using the baseline correlation
    # matrices
    avg_cormats <- erp_avg_firstn_cormats(
      cormats = baseline_src_mat
      , n = avg_n
    )
  }

  # Calculate the matrix difference to the src matrix
  rem_diff <- corr_rem %>%
    purrr::map(.x = ., ~ get_mat_diff(
      src_mat = avg_cormats
      , comp_mat = .x
    ))

  base::return(rem_diff)
}

# -------------------------------------------------------------------------------
# FISHER TRANSFORMATION related calculations
#   - variance stabilized threshold calculations
#   - matrix thresholding function
# -------------------------------------------------------------------------------

#' Threshold values of a matrix based on upper and lower thresholds
#'
#' @param inp_mat
#' @param lower_th
#' @param upper_thr
#'
#' @return
#' @export
diff_mat_threshold <- function(inp_mat, lower_th = -0.9, upper_thr = 0.9) {
  inp_mat[inp_mat >= upper_thr] <- upper_thr
  inp_mat[inp_mat <= lower_th] <- lower_th
  base::return(inp_mat)
}

mat_abs_threshold_test <- function(inp_mat_abs, abs_thr) {
  inp_mat_abs[inp_mat_abs >= abs_thr] <- 0
  base::return(inp_mat_abs)
}


#' Obtain the 2-sided absolute Fisher information adjusted
#' threshold
#'
#' @param alph : overall alpha level (default value 0.05)
#' @param erp_filt_df : list of data frames containing filtered
#' ERP readings by channel, 1 data frame for each channel
#'
#' @return
#' @export
get_fisher_abs_thrld <- function(alph = 0.05, erp_filt_df) {

  # Number of features i.e. covariates
  d <- length(erp_filt_df)

  # Bonferroni Correction for alpha
  m <- base::choose(n = d, k = 2)

  # Bonferroni corrected alpha level
  alph_revised <- alph / (2 * m)

  # Sample size for calculating correlations
  n_size_total <- nrow(erp_filt_df[[1]])

  # For Fisher transform the sample size is reduced by 3 before taking the
  # square root
  n_size <- base::sqrt(n_size_total - 3)

  # Revised Fisher Transform Threshold (absolute) - 2 sided test
  rev_th <- abs(qnorm(p = alph_revised, mean = 0, sd = 1)) / (n_size)

  base::return(rev_th)
}

# -------------------------------------------------------------------------------
# Create correlation PLOTS - UNSMOOTHED
# TODO: Check if this is smoothed or unsmoothed
# -------------------------------------------------------------------------------

#' Title
#'
#' @param cat_idx
#' @param abs
#' @param fisher
#' @param time_val
#' @param smoothed
#'
#' @return
#' @export
create_crplot_title <- function(cat_idx, abs, fisher, time_val, smoothed = FALSE) {
  str_title <- stringr::str_c(
    stringr::str_to_title(cat_idx)
    , ":"
    , "Millisecond ="
    , stringr::str_c(stringr::str_pad(time_val, 2, pad = "0"), ",")
    , base::ifelse(abs, "Absolute,", "Non-Absolute,")
    , base::ifelse(fisher, "Fisher Transformed", "")
    , base::ifelse(smoothed, "Smoothed", "")
    , sep = " "
  ) %>%
    stringr::str_replace_all(
      string = ., pattern = "\\s{2,}"
      , replacement = ""
    ) %>%
    stringr::str_trim(string = ., side = "both")
  base::return(str_title)
}

#' Title
#'
#' @param cormat
#'
#' @return
#' @export
create_tidy_cormat <- function(cormat) {
  cor_tidy <- as.data.frame(cormat) %>%
    dplyr::mutate(channel_y = factor(
      row.names(.)
      , levels = row.names(.)
    )) %>%
    tidyr::gather(
      key = channel_x
      , value = value, -channel_y
      , na.rm = TRUE, factor_key = TRUE
    )
  base::return(cor_tidy)
}

#' Title
#'
#' @param inp_str
#' @param pad_pfx
#' @param pad_width
#' @param pad_str
#' @param pad_sep
#'
#' @return
#' @export
pad_erp_str <- function(inp_str, pad_pfx
                        , pad_width = 2, pad_str = "0", pad_sep = "_") {
  out_str <- stringr::str_c(
    pad_pfx
    , stringr::str_pad(
      inp_str, pad_width
      , pad = pad_str
    )
    , sep = "_"
  )
  base::return(out_str)
}

#' Title
#'
#' @param cat_idx
#' @param abs
#' @param fisher
#' @param time_val
#' @param file_ext
#'
#' @return
#' @export
create_crplot_name <- function(cat_idx, abs, fisher, time_val
                               , file_ext = ".png") {
  stringr::str_c(
    "crplot"
    , base::ifelse(abs, "abs", "")
    , base::ifelse(fisher, "fish", "")
    , "cat"
    , stringr::str_to_lower(cat_idx)
    , "ms"
    , stringr::str_c(
      stringr::str_pad(time_val, 2, pad = "0")
      , file_ext
    )
    , sep = "_"
  ) %>%
    stringr::str_replace_all(
      string = ., pattern = "\\_{2,}"
      , replacement = "_"
    ) %>%
    base::return()
}

#' Title
#'
#' @param time_idx
#' @param n_patient_num
#' @param batch_idx
#' @param cat_idx
#' @param session_idx
#' @param smoothed
#' @param out_dir
#' @param diff_type
#' @param abs
#' @param fisher
#' @param plot_gg
#' @param thr_type
#'
#' @return
#' @export
erp_save_plot <- function(time_idx
                          , n_patient_num
                          , batch_idx
                          , cat_idx
                          , session_idx
                          , smoothed
                          , out_dir
                          , diff_type = "int"
                          , abs = TRUE
                          , fisher = TRUE
                          , plot_gg
                          , thr_type = NULL) {
  batch_pad <- pad_erp_str(
    inp_str = batch_idx, pad_pfx = "batch"
    , pad_width = 2, pad_str = "0", pad_sep = "_"
  )

  n_patient_num_pad <- pad_erp_str(
    inp_str = n_patient_num, pad_pfx = "P"
    , pad_width = 2, pad_str = "0", pad_sep = "_"
  )

  session_idx_pad <- pad_erp_str(
    inp_str = session_idx, pad_pfx = "session"
    , pad_width = 2, pad_str = "0", pad_sep = "_"
  )

  plot_dir0 <- base::file.path(
    out_dir, n_patient_num_pad
    , session_idx_pad, batch_pad
    , stringr::str_to_lower(cat_idx)
  )

  if (smoothed & diff_type == "non") {
    plot_dir <- base::file.path(plot_dir0, "sm")
  } else if (!smoothed & diff_type == "non") {
    plot_dir <- base::file.path(plot_dir0, "us")
  } else if (smoothed & diff_type == "int") {
    plot_dir <- base::file.path(plot_dir0, "sm", "avg_cormat", "int_ps")
  } else if (smoothed & diff_type == "all") {
    plot_dir <- base::file.path(plot_dir0, "sm", "avg_cormat", "all_ps")
  } else if (!smoothed & diff_type == "int") {
    plot_dir <- base::file.path(plot_dir0, "us", "avg_cormat", "int_ps")
  } else if (!smoothed & diff_type == "all") {
    plot_dir <- base::file.path(plot_dir0, "us", "avg_cormat", "all_ps")
  } else if (smoothed & diff_type == "all_session") {
    plot_dir <- base::file.path(plot_dir0, "sm", "avg_cormat", "all_session_ps")
  } else if (!smoothed & diff_type == "all_session") {
    plot_dir <- base::file.path(plot_dir0, "us", "avg_cormat", "all_session_ps")
  }

  # If we are thresholding, then append that to the end of the save path
  if (!base::is.null(thr_type)) {
    plot_dir <- base::file.path(plot_dir, thr_type)
  }

  print(plot_dir)

  # # Create the plot name
  plot_name <- create_crplot_name(
    cat_idx = cat_idx
    , abs = abs
    , fisher = fisher
    , time_val = time_idx
    , file_ext = ".png"
  )
  print(plot_name)

  ggsave(
    filename = plot_name
    , plot = plot_gg
    , device = "png"
    , width = 15, height = 15, units = "cm"
  )

  # Move the plot png to the specified batch folder
  base::file.rename(
    from = plot_name
    , to = file.path(plot_dir, plot_name)
  )
}

#' Title
#'
#' @param inp_cormat
#' @param cat_idx
#' @param time_ms_idx
#' @param abs
#' @param fisher
#' @param plot_col_range
#' @param smoothed
#' @param diff
#'
#' @return
#' @export
create_corr_ts <- function(inp_cormat, cat_idx, time_ms_idx
                           , abs = TRUE, fisher = TRUE
                           , plot_col_range = c(-0.2, 2.5)
                           , smoothed = FALSE
                           , diff = FALSE) {
  print(time_ms_idx)

  # Create the plot name
  plot_title <- create_crplot_title(
    cat_idx = cat_idx
    , abs = abs
    , fisher = fisher
    , time_val = time_ms_idx
    , smoothed = smoothed
  )
  print(plot_title)


  # Pre-threshold matrices for plotting based on axes range
  inp_cormat <- diff_mat_threshold(
    inp_mat = inp_cormat
    , lower_th = plot_col_range[1]
    , upper_thr = plot_col_range[2]
  )

  # Convert to a tidy data frame for ggplot purposes
  cormat_tidy <- create_tidy_cormat(cormat = inp_cormat)

  legend_name <- stringr::str_c(
    "Abs. Fish. Corr"
    , ifelse(diff, "Diff", "")
    , sep = " "
  ) %>% stringr::str_trim()
  # Create the plot of the correlation Matrix
  p1 <- ggplot2::ggplot(
    data = cormat_tidy
    , aes(channel_x, channel_y, fill = value)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = "red", high = "blue"
      , mid = "white",
      midpoint = 0, limit = plot_col_range
      , space = "Lab",
      name = legend_name
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = element_text(
        angle = 90
        , vjust = 1,
        size = 6
        , hjust = 1
      )
      , axis.text.y = element_text(size = 6)
    ) +
    ggplot2::labs(
      title = plot_title
      , subtitle = "Correlation across neural Channels"
      , x = "Neural Channel"
      , y = "Neural Channel"
    ) +
    ggplot2::coord_fixed()

  base::return(p1)
}

# -------------------------------------------------------------------------------
# INPUT: Read in raw ERP data
# -------------------------------------------------------------------------------

#' Rbind session data frames across 2 lists containing dataframes
#' for each individual session
#'
#' @param erp_df_lab1
#' @param erp_df_lab2
#'
#' @return
#' @export
erp_rbind_sessions <- function(erp_df_lab1, erp_df_lab2) {
  base::return(purrr::map2(
    .x = erp_df_lab1
    , .y = erp_df_lab2
    , ~ rbind(.x, .y)
  ))
}

erp_create_df_labeled <- function(n_data_type,
                                  n_patient_num,
                                  session_index,
                                  task,
                                  filter,
                                  channel_index) {

  # Output directory to unzip the tar file
  out_dir_path <- stringr::str_c("P", n_patient_num, "_fullset") %>%
    here::here(n_data_type, .)
  out_dir_path

  # GET the list of all files in the unzipped folder
  all_erp_files <- neuroada::get_erp_files(erp_dir = out_dir_path)
  dim(all_erp_files)

  # Filter down the list of all ERP files to the ERP Label datasets only
  erp_labels_files <-
    all_erp_files %>%
    dplyr::filter(label_type != "err" &
      session_num == session_index &
      task_type == task) %>%
    dplyr::select(full_path, label_type)

  # Combine the labels into a single tibble
  all_erp_labels <- purrr::pmap(
    erp_labels_files
    , neuroada::read_erp_label_file
  ) %>%
    purrr::map_dfc(., cbind) %>%
    dplyr::mutate(
      lab_category = base::as.character(lab_category),
      lab_category =
        # TODO: We need to auto-join these factors
      # Should not be hardcoded like this
      forcats::fct_recode(
        lab_category
        , "Bodies" = "1"
        , "Faces" = "2"
        , "Words" = "3"
        , "Hammers" = "4"
        , "Houses" = "5"
        , "Nonobjects" = "6"
      )
    )
  dim(all_erp_labels)

  # ERP time series files
  erp_nonlabels_files <- all_erp_files %>%
    dplyr::filter(!is_ds_store &
      task_type == task &
      filt_type == filter &
      session_num == session_index &
      channel_num <= channel_index) %>%
    dplyr::select(
      full_path
      , task_type
      , session_num
      , filt_type
      , channel_num
    ) %>%
    dplyr::arrange(
      task_type
      , session_num
      , filt_type
      , channel_num
    )

  # -------------------------------------------------------------------------------
  # All Channels: Read all ERP Channel Data with Labels
  # -------------------------------------------------------------------------------

  # READ all erp data files into a single list, each element is from a
  # single channel
  erp_df_list <- purrr::pmap(erp_nonlabels_files, neuroada::read_erp_file)

  # cbind all of the labels to each of the channel datafiles that are read in
  erp_df_labeled <- purrr::map(erp_df_list, ~ cbind(all_erp_labels, .x))
  length(erp_df_labeled)

  base::return(erp_df_labeled)
}

# -------------------------------------------------------------------------------
# Create all covariance and correlation matrices by category
# -------------------------------------------------------------------------------

#' Title
#'
#' @param erp_df_labeled
#' @param core_cat
#' @param all_erp_time_int
#' @param bin_width
#'
#' @return
#' @export
erp_create_cat_all_mats <- function(erp_df_labeled, core_cat,
                                    all_erp_time_int, bin_width) {

  # ---------------------------------------------------------------------------
  # Run the Core Functionality
  # ---------------------------------------------------------------------------

  # CATEGORY : Filtered
  erp_filt_df_cat <- erp_df_labeled %>%
    erp_create_filter_df(
      inp_erp_df_lab = .
      , cat_idx = core_cat
    )
  # ALL : Unfiltered
  erp_filt_df_all <- erp_df_labeled %>%
    erp_create_filter_df(
      inp_erp_df_lab = .
      , cat_idx = NULL
    )

  # ---------------------------------------------------------------------------
  # COVARIANCE MATRICES: Computation
  # ---------------------------------------------------------------------------

  # Covariance Matrices Unsmoothed FACES
  erp_covmats_us_cat <- create_covmats(
    inp_erp_filt_df_lab = erp_filt_df_cat
    , time_int = all_erp_time_int
    , cat_idx = core_cat
  )

  # Covariance Matrices Unsmoothed ALL
  erp_covmats_us_all <- create_covmats(
    inp_erp_filt_df_lab = erp_df_labeled
    , time_int = all_erp_time_int
    , cat_idx = NULL
  )

  # Covariance Matrices Smoothed CATEGORY
  erp_covmats_sm_cat <-
    erp_ksmooth_matrix_list(mat_list = erp_covmats_us_cat, bwth = bin_width)

  # Covariance Matrices Smoothed ALL
  erp_covmats_sm_all <-
    erp_ksmooth_matrix_list(mat_list = erp_covmats_us_all, bwth = bin_width)

  # ---------------------------------------------------------------------------
  # CORRELATION MATRICES: Computation
  # ---------------------------------------------------------------------------

  # Correlation Matrices Unsmoothed CATEGORY
  erp_cormats_us_cat <- erp_covmats_us_cat %>%
    purrr::map(
      .x = .
      , ~ erp_cov2corr(
        inp_covmat = .x
        , abs = TRUE
        , fisher = TRUE
      )
    )

  # Correlation Matrices Unsmoothed ALL
  erp_cormats_us_all <- erp_covmats_us_all %>%
    purrr::map(
      .x = .
      , ~ erp_cov2corr(
        inp_covmat = .x
        , abs = TRUE
        , fisher = TRUE
      )
    )

  # Correlation Matrices Smoothed CATEGORY
  erp_cormats_sm_cat <- erp_covmats_sm_cat %>%
    purrr::map(
      .x = .
      , ~ erp_cov2corr(
        inp_covmat = .x
        , abs = TRUE
        , fisher = TRUE
      )
    )

  # Correlation Matrices Smoothed ALL
  erp_cormats_sm_all <- erp_covmats_sm_all %>%
    purrr::map(
      .x = .
      , ~ erp_cov2corr(
        inp_covmat = .x
        , abs = TRUE
        , fisher = TRUE
      )
    )

  out_list <- base::list(
    "erp_filt_df_cat" = erp_filt_df_cat,
    "erp_filt_df_all" = erp_filt_df_all,
    "erp_covmats_us_cat" = erp_covmats_us_cat,
    "erp_covmats_us_all" = erp_covmats_us_all,
    "erp_covmats_sm_cat" = erp_covmats_sm_cat,
    "erp_covmats_sm_all" = erp_covmats_sm_all,
    "erp_cormats_us_cat" = erp_cormats_us_cat,
    "erp_cormats_us_all" = erp_cormats_us_all,
    "erp_cormats_sm_cat" = erp_cormats_sm_cat,
    "erp_cormats_sm_all" = erp_cormats_sm_all
  )

  base::return(out_list)
}