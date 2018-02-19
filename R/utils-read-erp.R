# -------------------------------------------------------------------------------
# Utilities to enable the easy read in of ERP data
# -------------------------------------------------------------------------------

#' Detect whether a regex pattern exists in a given input string
#'
#' @param inp_str The input string in which to detect a pattern
#' @param str_pattern The regex pattern to detect in the input string
#' @param ignore_case Logical. When `TRUE` and in an interactive session case
#'   will be ignored in the regex pattern detection. This is the default.
#'
#' @return Logical. `TRUE` if the pattern is detected in the string
#' @export
detect_pattern <- function(inp_str, str_pattern, ignore_case = TRUE) {
  inp_str %>%
    stringr::str_detect(
      string = .
      , pattern = regex(
        str_pattern
        , ignore.case = ignore_case
      )
    ) %>%
    base::return()
}

#' Extract text and replace it based on a specified pattern
#'
#' @param inp_str The input string in which to extract a pattern
#' @param ext_pat The regex pattern to extract from the input string
#' @param sub_pat The regex pattern to substitute in the extracted string
#' @param repl_pat The replacement string to substitute in the extracted string
#' @param ignore_case Logical. When `TRUE` and in an interactive session case
#'   will be ignored in the regex pattern detection. This is the default.
#'
#' @return Character. The extracted string with the substituted pattern
#' @export
ext_repl_pattern <- function(inp_str, ext_pat, sub_pat, repl_pat = ""
                             , ignore_case = TRUE) {
  inp_str %>%
    stringr::str_extract(
      string = .
      , pattern = regex(
        ext_pat
        , ignore.case = ignore_case
      )
    ) %>%
    stringr::str_replace(
      string = ., pattern = sub_pat
      , replacement = repl_pat
    ) %>%
    as.integer() %>%
    base::return()
}

#' Parses out the document name from a filepath excluding the extension of
#' the document.
#' This is a helper function for the \code{analyzecorpus} function
#'
#' @param document_path The filepath of a document or just the document name
#' (with or without extension)
#'
#' @return string containing the document name without the extension
#' of the document
#' @export
#'
#' @examples
#' doc_path <- "/Users/test/testing_doc.txt"
#' # The following function application returns "testing_doc"
#' get_document_name(doc_path)
get_document_name <- function(document_path) {
  out_docname <- base::basename(document_path) %>%
    stringr::str_split(string = ., pattern = "\\.") %>%
    purrr::map_chr(1)
  return(out_docname)
}

#' Used to generate a formatted tibble of erp files and filtering flags
#'
#' @param erp_dir The directory of erp files for a particular patient
#'
#' @return Tibble. formatted tibble of erp files and filtering flags
#' @export
get_erp_files <- function(erp_dir) {
  # Get a list of all of the files
  all_erp_files <- tibble::tibble(
    files =
      list.files(
        path = erp_dir
        , recursive = TRUE
      )
  )
  all_erp_files <-
    all_erp_files %>%
    dplyr::mutate(
      is_ds_store = detect_pattern(inp_str = files, str_pattern = ".*DS_Store.*")
      , filt_type = case_when(
        detect_pattern(inp_str = files, str_pattern = ".*bandpass.*") ~ "bandpass",
        detect_pattern(inp_str = files, str_pattern = ".*raw.*") ~ "raw",
        TRUE ~ "err"
      )
      , task_type = case_when(
        detect_pattern(inp_str = files, str_pattern = ".*category.*") ~ "category",
        detect_pattern(inp_str = files, str_pattern = ".*individuation.*") ~ "individuation",
        TRUE ~ "err"
      )
      , session_num = ext_repl_pattern(
        inp_str = files, ext_pat = "session\\d+"
        , sub_pat = "session", ignore_case = TRUE
      )
      , channel_num = ext_repl_pattern(
        inp_str = files, ext_pat = "ch\\d+"
        , sub_pat = "ch", ignore_case = TRUE
      )
      , full_path = base::file.path(erp_dir, files)
      , doc_name = get_document_name(files)
      , label_type = case_when(
        detect_pattern(inp_str = doc_name, str_pattern = ".*Category_label.*") ~ "lab_category",
        detect_pattern(inp_str = doc_name, str_pattern = ".*Expression_label.*") ~ "lab_expression",
        detect_pattern(inp_str = doc_name, str_pattern = ".*Face_label.*") ~ "lab_face",
        detect_pattern(inp_str = doc_name, str_pattern = ".*Gaze_label.*") ~ "lab_gaze",
        detect_pattern(inp_str = doc_name, str_pattern = ".*Gender_label.*") ~ "lab_gender",
        detect_pattern(inp_str = doc_name, str_pattern = ".*goodtrial_label.*") ~ "lab_goodtrial",
        detect_pattern(inp_str = doc_name, str_pattern = ".*Individuation_timestamps.*") ~ "lab_ind_timestamps",
        detect_pattern(inp_str = doc_name, str_pattern = ".*Category_timestamps.*") ~ "lab_cat_timestamps",
        TRUE ~ "err"
      )
    )

  return(all_erp_files)
}

#' Read in the specified erp label files
#'
#' @param full_path The filepath to the specified erp label file
#' @param label_type The erp label type associated with the erp label file.
#'   This will form the header for the erp file tibble once it is read
#'   in
#'
#' @return Tibble. The erp label file with the erp label type as the header
#' @export
read_erp_label_file <- function(full_path, label_type) {
  rf <- readr::read_csv(
    file = full_path
    , col_names = FALSE
    , col_types = cols(
      X1 = col_double()
    )
  )
  colnames(rf) <- label_type
  return(rf)
}

#' Read in the specified erp data files (non-labels)
#'
#' @param full_path The filepath to the specified erp label file
#' @param session_num The session number associated with each erp data file
#'   i.e. typically {1, 2}
#' @param channel_num The channel number associated with each erp data file
#'   i.e. typically {1, 2, ... 85}
#' @param filt_type The filter type associated with each erp data file
#'   i.e. typically {"raw", "bandpass"}
#' @param task_type The task type associated with each erp data file
#'   i.e. typically {"category", "individuation"}
#'
#' @return Tibble. The erp data file with session number, channel number,
#'   filter type, task type and erp voltages for each millisecond X1:X1500
#' @export
read_erp_file <- function(full_path, session_num, channel_num, filt_type
                          , task_type) {
  rf <- readr::read_csv(
    file = full_path
    , col_names = FALSE
    , col_types = cols(
      X1 = col_double()
    )
  ) %>%
    dplyr::mutate(
      session_idx = base::as.factor(session_num)
      , channel_idx = base::as.factor(channel_num)
      , filt_type = base::as.factor(filt_type)
      , task_type = base::as.factor(task_type)
    ) %>%
    dplyr::select(
      session_idx
      , channel_idx
      , filt_type
      , task_type
      , dplyr::everything()
    )
  return(rf)
}

#' Untar a tar.gz file to a specified output path
#'
#' @param targz_filepath The filepath to the tar.gz file
#' @param ext_path The filepath to extract the contents of the
#'   \code{targz_filepath} file
#'
#' @export
unrar_erp_file <- function(targz_filepath, ext_path) {
  untar(
    tarfile = targz_filepath
    , exdir = ext_path
  )
}