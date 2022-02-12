#!/usr/bin/env Rscript


#######################################################################################################
##                                                                                                   ##
##  Script name: invoice_to_xlsx_parser.R                                                            ##
##  Purpose of script: Parse N-able distributor invoices (eg. B&R or IT-pro) from pdf to xlsx.       ##
##                                                                                                   ##
##  Notes: Use at your own peril.                                                                    ##
##         Parsing pdf-s is a messy business, so there are many ways for this script to break,       ##
##         most of which are yet to be discovered and fixed.                                         ##
##                                                                                                   ##
##  Author: Mart Roben                                                                               ##
##  Date Created: 12. Feb 2022                                                                       ##
##                                                                                                   ##
##  Copyright: BSD-3-Clause                                                                          ##
##  https://github.com/martroben/nable_scripts                                                       ##
##                                                                                                   ##
##  Contact: mart@altacom.eu                                                                         ##
##                                                                                                   ##
#######################################################################################################


#################
# Load packages #
#################

if(!require("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  magrittr,
  tibble,
  dplyr,
  stringr,
  purrr,
  rlang,
  pdftools,
  openxlsx,
  argparser)



##########
# Inputs #
##########

p <- argparser::arg_parser("This script parses N-able pdf invoices to xlsx.", hide.opts = TRUE)
p <- argparser::add_argument(p, "--invoice_pdf", help = "Input pdf full path.")

input_args <- argparser::parse_args(p)
input_pdf_path <- input_args$invoice_pdf

default_output_xlsx_path <- file.path(
  dirname(input_pdf_path),
  stringr::str_replace(basename(input_pdf_path), "\\.pdf", ".xlsx"))

output_xlsx_path <- default_output_xlsx_path



###############
# Import data #
###############

pdf_contents <- pdftools::pdf_text(input_pdf_path)



####################
# Global variables #
####################

# Column names that are known to be used in summary and usage details.
known_column_names <<- c(
  "Service",
  "From",
  "To",
  "Quantity",
  "List Price",
  "Charge Price",
  "Currency",
  "Cost",
  "Description",
  "Service From",
  "Service To",
  "Rate",
  "Total Cost")


# Threshold used to determine if parsed values and report values match.
accuracy_threshold <<- 0.1


# "€" symbol in unicode, because it could get messed up with different encodings.
eur_unicode <<- "\u20AC"
# Different possible dash symbols that could be used for minus sign on report.
dash_unicode <<- c("\u002D", "\u2010", "\u2011", "\u2012", "\u2013", "\u2014", "\u2015", "\uFE58", "\uFF0D")


# Optional "-" symbol, optional "€" symbol, optional "x,xx",
# obligatory "x.xx", followed by line break or end of string.
currency_pattern <<- stringr::str_c(
  ".*?[", stringr::str_flatten(.GlobalEnv$dash_unicode), "]?",
  .GlobalEnv$eur_unicode, "?",
  "\\d*?,?\\d+\\.\\d{2}.*?(?:\\n|$)")

# Assuming that column name "Currency" is always present on report.
column_names_line_pattern <<- "\\n.*?Currency.*?\\n"



#############
# Functions #
#############

extract_report_name <- function(usage_entry_string) {
  # First line of string with word characters.
  
  name_pattern <- "^\\n*(.*?)\\n"
  
  usage_entry_string %>%
    stringr::str_match(name_pattern) %>%
    magrittr::extract(2) %>%
    return()
}


get_column_names_string <- function(usage_strings) {
  # Extract column names line from summary or a vector of usage entries.
  # Loop handles situation where first usage entry don't have
  # column names.
  
  for(i in 1:length(usage_strings)) {
    
    column_names_string <- usage_strings[i] %>%
      stringr::str_extract(
        stringr::str_glue("(?i){.GlobalEnv$column_names_line_pattern}")) %>%
      magrittr::extract(1) %>%
      stringr::str_trim()
    
    if (!is.na(column_names_string)) break()
  }
  return (column_names_string)
}


extract_longest_string <- function(string) {
  # Given a string vector, returns the string with most words.
  
  if (length(string) == 0) {
    
    return(string)
    
  } else {
  
    string %>%
      magrittr::extract(
        which(
          stringr::str_count(., "\\b") == max(stringr::str_count(., "\\b")))) %>%
      return()
  }
}


extract_column_names <- function(column_names_string, known_column_names) {
  # Extracts individual column names from column names line,
  # using known column names defined in input.
  # Can handle single unknown column names between known ones.
  
  unmatched_column_names <- known_column_names
  matched_column_names <- character(0)
  unknown_name <- character(0)
  
  while(stringr::str_length(column_names_string) > 0) {
    
    # Find match from known column names.
    # If several matches, choose the one with most words
    # (eg. "Service From", not "Service").
    match <- unmatched_column_names %>%
      stringr::str_c("(?i)^", ., "(?:\\s|$)") %>%
      stringr::str_detect(column_names_string, .) %>%
      which() %>%
      magrittr::extract(unmatched_column_names, .) %>%
      extract_longest_string()
    
    if (length(match) == 1) {
      
      if (length(unknown_name) != 0) {
        
        matched_column_names <- c(
          matched_column_names,
          stringr::str_trim(unknown_name))
        
        unknown_name <- character(0)
      }
      
      matched_column_names <- c(matched_column_names, match)
      unmatched_column_names <- unmatched_column_names %>%
        purrr::discard(
          ~stringr::str_detect(
            string = .x, 
            pattern = stringr::str_glue("(?i)^{match}$")))
      
      column_names_string <- column_names_string %>%
        stringr::str_remove(
          string = .,
          pattern = stringr::str_c("(?i)^", match, "\\s*\\b"))
      
    } else {
      
      unknown_name <- stringr::str_c(
        unknown_name,
        stringr::str_extract(column_names_string, "^\\w*(\\s|$)"))
      
      column_names_string <- stringr::str_remove(column_names_string, "^\\w*\\s*\\b")
    }
  }
  
  column_names <- c(matched_column_names, unknown_name)
  return (column_names)
}


currency_to_numeric <- function(currency) {
  # Removes eur symbol and thousands separators.
  # Detects if amount is negative.
  # Vectorised.
  
  to_num <- function(currency) {
    
    if (!is.na(currency) && currency == " ") currency <- NA_real_
    
    is_negative <- currency %>%
      stringr::str_detect(dash_unicode) %>%
      any()
    
    chars_to_remove <- c(.GlobalEnv$eur_unicode, .GlobalEnv$dash_unicode, ",")
    
    numeric_currency <- stringr::str_remove_all(
      string = currency,
      pattern = stringr::str_flatten(chars_to_remove, collapse = "|")) %>%
      as.numeric() %>%
      purrr::when(
        is_negative ~ .*(-1),
        ~ .)
    
    return (numeric_currency)
  }
  
  purrr::map_dbl(currency, to_num) %>%
    return()
}


extract_grand_totals <- function(usage_entry_string) {
  # Grand total: currency string after "Grand Total Amount".
  
  grand_total_pattern <- stringr::str_c(
    "^[\\S\\s]*?Grand Total Amount.*\\n(", .GlobalEnv$currency_pattern, ")")
  
  usage_entry_string %>%
    stringr::str_match(grand_total_pattern) %>%
    magrittr::extract(2) %>%
    stringr::str_trim() %>%
    currency_to_numeric() %>%
    return()
}


get_usage_lines <- function(usage_entry_string) {
  # Remove column names and totals from usage entry.
  # Divide it into a string vector by lines.
  
  # Front trim: from beginning of string
  # to the end of column name line.
  front_trim_pattern <- stringr::str_c(
    "(?i)^[\\S\\s]*?",
    .GlobalEnv$column_names_line_pattern)
  
  # Back trim: From line that has "Total" (optinally preceeded by
  # single white space and another word) to the end of string."
  back_trim_pattern <- "\n\\s*?\\w*? Total[\\S\\s]*$"
  
  usage_entry_string %>% 
    stringr::str_remove(front_trim_pattern) %>%
    stringr::str_replace_all(.GlobalEnv$column_names_line_pattern, "\n") %>%
    str_remove(back_trim_pattern) %>%
    stringr::str_split("\n") %>%
    purrr::pluck(1) %>%
    return()
} 


is_currency_line <- function(pdf_line) {
  # TRUE if line contains currency pattern.
  # FALSE if input is NA
  
  if (length(pdf_line) == 1 && is.na(pdf_line)) {
    
    return (FALSE)
    
  } else {
  
    stringr::str_detect(
      string = pdf_line,
      pattern = .GlobalEnv$currency_pattern) %>%
      return()
  }
}


is_multiline_description <- function(pdf_line) {
  # Detect 2nd description line as line consisting of words that are
  # separated by no more than one white space.
  
  stringr::str_detect(
    string = pdf_line,
    pattern = "(^$)|(\\b\\s{2,}\\b)",
    negate = TRUE) %>%
    return()
}


handle_missing_values <- function(problem_line, column_names) {

  columns_vector <- problem_line %>%
    stringr::str_split("\\s{2,}") %>%
    purrr::pluck(1)
  
  # Assume that missing element is in the position
  # with the most white spaces between values.
  missing_element_position <- problem_line %>%
    stringr::str_match_all("\\s{2,}") %>%
    purrr::pluck(1) %>%
    stringr::str_length() %>%
    {which(. == max(.))}
  
  # Pad missing position with a white space.
  columns_vector <- append(
    x = columns_vector,
    values = " ",
    after = missing_element_position)
  
  # If vector length still doesn't match number of columns,
  # keep only first and last values and replace/pad others with spaces.
  if (length(columns_vector) != length(column_names)) {
    
    columns_vector <- c(
      columns_vector[1],
      rep(" ", length(column_names) - 2),
      columns_vector[length(columns_vector)])
  }
  return (columns_vector)
}


get_usage_table <- function(usage_lines, column_names) {
  # Turn usage line strings to a table.
  # Try to handle cases where there are line breaks in description column.
  
  usage_entry_table <- tibble::tibble()
  
  for (i in 1:length(usage_lines)) {
    
    if (is_currency_line(usage_lines[i])) {
      
      # Detect different column entries by 2 or more white spaces between.
      # Pad any numeric parts of string (with two decimals) with spaces to reduce parsing errors.
      columns_vector <- usage_lines[i] %>%
        stringr::str_replace_all("\\S*?\\d\\S*?\\.\\d{2,}", " \\0 ") %>%
        stringr::str_split("\\s{2,}") %>%
        purrr::pluck(1)
      
      if (length(columns_vector) < length(column_names)) {
        
        columns_vector <- handle_missing_values(usage_lines[i], column_names)
      }
        
      usage_entry_table <- columns_vector %>%
        tibble::as_tibble_row(.name_repair = ~column_names) %>%
        dplyr::bind_rows(usage_entry_table, .)
      
    } else if (is_multiline_description(usage_lines[i]) &&
               i > 1 &&
               is_currency_line(usage_lines[i-1])) {
      
      usage_entry_table[nrow(usage_entry_table), 1] <- stringr::str_c(
        usage_entry_table[nrow(usage_entry_table), 1],
        usage_lines[i],
        sep = " ")
    }
  }
  return (usage_entry_table)
}


detect_numeric <- function(x) {
  # Detect numbers by seeing if string is parseable to numeric
  # without returning NA.
  
  as.numeric(x) %>%
    suppressWarnings() %>%
    {is.numeric(.) && !is.na(.)} %>%
    return()
}


to_numeric <- function(x) {
  # Interpret single white space as 0 when turning
  # strings to numbers.
  
  ifelse(
    !is.na(x) && x == " ",
    yes = return (0),
    no = return (as.numeric(x)))
}


set_numeric_columns <- function(usage_table) {
  # Parse numbers from currency strings and set numeric columns to numeric type.
  
  usage_table %>%
    dplyr::mutate(
      across(
        .cols = where(~any(is_currency_line(.x), na.rm = TRUE)),
        .fns = ~currency_to_numeric(.x)),
      across(
        .cols = where(~any(detect_numeric(.x), na.rm = TRUE)),
        .fns = ~to_numeric(.x))) %>%
    return()
}


get_total_cost <- function(usage_table) {
  # Get total cost for one usage entry.
  # Return NA if there are no usage lines
  
  if (ncol(usage_table) == 0) {
    
    return (NA_real_)
    
  } else {
    
    usage_table %>%
      pull(ncol(.)) %>%
      sum() %>%
      return()
  }
}


get_NA_with_type <- function(type) {
  # Enter type name as string and get NA of the required type.
  
  type %>%
    stringr::str_c("as.", .) %>%
    {get(.)(NA)} %>%
    return()
}


get_most_frequent <- function(vec) {
  # Get most frequent element of vector.
  
  vec %>%
    table() %>%
    {which(. == max(.))} %>%
    names() %>%
    first() %>%
    return()
}


create_placeholder_usage_item <- function(common_column_types, grand_total) {
  # Create usage item with total usage.
  
  empty_usage_item <- common_column_types %>%
    purrr::map_dfc(~get_NA_with_type(.x))
  
  empty_usage_item %>%
    dplyr::mutate(
      across(1, ~"Total"),
      across(ncol(.), ~grand_total)) %>%
    return()
}


apply_common_type <- function(col, col_name, common_col_types) {
  # Helper function to be used with dplyr::mutate(across()).
  # Compiles a type formatting function (as.character(), as.numeric(), etc.)
  # and calls it on input column.
  
  common_col_types[[col_name]] %>%
    stringr::str_c("as.", .) %>%
    do.call(list(col)) %>%
    return()
}


force_common_column_types <- function(table, common_col_types) {
  # Apply common column types to a table
  
  table %>%
    dplyr::mutate(
      across(
        .cols = everything(),
        .fns = ~apply_common_type(.x, cur_column(), common_col_types))) %>%
    return()
}


table_to_padded_string <- function(table) {
  # Select first and last column of table and parse it to a printable string.
  
  padded_length <- table %>%
    dplyr::pull(1) %>%
    stringr::str_length() %>%
    max() %>%
    magrittr::add(2)
  
  purrr::map_chr(
    .x = 1:nrow(table),
    .f = ~stringr::str_c(
      stringr::str_pad(table[.x, 1], padded_length, side = "right"),
      table[.x, ncol(table)], sep = " ")) %>%
    stringr::str_flatten("\n") %>%
    return()
}


ask_replace_file <- function(path) {
  # Ask user interactively if they want to replace file at input path
  
  # Temporarily set to print warnings immediately
  existing_setting <- options("warn")
  on.exit(options(existing_setting))
  options(warn = 1)
  
  warning_msg <- c(
    stringr::str_glue(
      "File named '{basename(path)}' already exists at
       {dirname(path)}
       Would you like to replace it?"),
    "'Y' = yes",
    "'N' = no")
  
  rlang::warn(warning_msg)
  
  readline("selection: ") %>%
    stringr::str_detect("(?i)Y") %>%
    return()
}


abort_quietly <- function() {
  # Stop script without showing an error.
  
  existing_setting <- options(show.error.messages = FALSE)
  on.exit(options(existing_setting))
  rlang::abort()
}



######################################
# Separate summary and usage entries #
######################################

pdf_string <- stringr::str_c(pdf_contents, collapse = "___PAGE_BREAK___")


# Summary detected by "Summary" in the start and
# "Total" + currency string in the end.
summary_pattern <- stringr::str_c(
  "(?i)^[\\S\\s]*?",
  "(Summary[\\S\\s]*?Total", .GlobalEnv$currency_pattern, ")",
  "([\\S\\s]*$)")

summary_matches <- pdf_string %>%
  stringr::str_match(summary_pattern)

summary_string <- summary_matches[,2] %>%
  stringr::str_remove("___PAGE_BREAK___")

# Usage entries start at the beginning of next page after summary.
all_usage_entries_string <- summary_matches[,3] %>%
  stringr::str_remove("[\\S\\s]*?___PAGE_BREAK___") %>%
  stringr::str_remove_all("___PAGE_BREAK___")



##########################
# Separate usage entries #
##########################

# Usage entry detected as pattern from start of string
# until "Grand Total Amount" + currency string.
usage_entry_pattern <- stringr::str_c(
  "(?i)^([\\S\\s]*?",
  "Grand Total Amount.*?\\n",
  .GlobalEnv$currency_pattern, ")",
  "([\\S\\s]*$)")

remaining_usage_entries <- all_usage_entries_string
usage_entry_strings <- character(0)

while (stringr::str_detect(remaining_usage_entries, usage_entry_pattern)) {

  usage_entry_matches <- remaining_usage_entries %>%
    stringr::str_match(usage_entry_pattern)

  usage_entry_strings <- c(usage_entry_strings, usage_entry_matches[,2])
  remaining_usage_entries <- usage_entry_matches[,3]
}



##########################
# Extract names & totals #
##########################

# Extract company names
report_names <- usage_entry_strings %>%
  purrr::map_chr(extract_report_name)


# Extract totals.
grand_totals <- usage_entry_strings %>%
  purrr::map_dbl(extract_grand_totals) %>%
  rlang::set_names(report_names)


# Extract usage detail column names.
column_names <- usage_entry_strings %>%
  get_column_names_string() %>%
  extract_column_names(.GlobalEnv$known_column_names)



#########################################
# Extract client-specific usage details #
#########################################

# Detect usage lines by currency-like string in the end of line.
# Detect wrapped (multi-line) descriptions by
# words separated by 1 white space in beginning of next line.
# Detect numeric columns by any value being parsable to a number in the column.
usage_tables_raw <- list()
for (i in 1:length(usage_entry_strings)) {
  
  usage_tables_raw <- usage_entry_strings[i] %>%
    get_usage_lines() %>%
    get_usage_table(column_names) %>%
    set_numeric_columns() %>%
    list() %>%
    append(usage_tables_raw, .)
}


# Get (most frequent) column types.
common_column_types <- usage_tables_raw %>%
  map_dfr(summarise_all, class) %>%
  dplyr::summarise(
    across(
      .cols = everything(),
      .fns = ~get_most_frequent(.x))) %>%
  c(recursive = TRUE)


# Insert placeholders for usage tables with 0 rows.
usage_tables <- purrr::pmap(
  .l = list(usage_tables_raw, grand_totals),
  .f = ~when(
    .x,
    nrow(.x) == 0 ~ create_placeholder_usage_item(common_column_types, .y),
    ~ .)) %>%
  purrr::map(~force_common_column_types(.x, common_column_types)) %>%
  rlang::set_names(report_names)


# Combine usage details to single table.
all_usages_table <- purrr::pmap_dfr(
    .l = list(usage_tables, names(usage_tables)),
    .f = ~mutate(.x, Name = .y, .before = 1)) %>%
  dplyr::mutate(
    across(
      .cols = everything(),
      .fns = ~dplyr::na_if(.x, " ")))



#####################################################################
# Check if parsed usage line totals and report 'grand totals' match #
#####################################################################

# Get companies where usage lines don't match grand total.
check_if_totals_match <- function(usage_table, grand_total) {
  
  usage_table_total <- get_total_cost(usage_table)
  totals_match <- abs(usage_table_total - grand_total) < .GlobalEnv$accuracy_threshold
}

non_matching_totals <- purrr::pmap_lgl(
  .l = list(usage_tables, grand_totals),
  .f = ~check_if_totals_match(.x, .y)) %>%
  not() %>%
  which()


# Print warning if non-matching totals found.
if (length(non_matching_totals) > 0) {
  
  non_matching_info <- purrr::map_chr(
      .x = non_matching_totals,
      .f = ~ stringr::str_c(
         strrep("-", 61), "\n",
         report_names[.x], "\n",
         table_to_padded_string(usage_tables[[.x]]),
         "\nUsage lines total: ", get_total_cost(usage_tables[[.x]]),
         "\nGrand total on report: ", grand_totals[.x])) %>%
    stringr::str_flatten("\n")
  
  warning_str <- stringr::str_c(
    "The following Grand Totals and usage line totals don't match:\n",
    non_matching_info)
  
  rlang::warn(warning_str)
  
} else {
  
  info_msg <- c(
    "Checked if usage line totals and reported Grand Totals match for each client.",
     i = "All good!")
  
  rlang::inform(info_msg)
}



###########
# Summary #
###########

# Get summary total

# Summary total: line with "Total" followed by currency string.
# Optionaly there could be a white space and another word in front of "Total".
summary_total_pattern <- stringr::str_c(
  "(?i)\\n.*?\\w*?\\s?Total:?.*?",
  "(", currency_pattern, ")")

summary_total_report <- summary_string %>%
  stringr::str_match(summary_total_pattern) %>%
  magrittr::extract(2) %>%
  stringr::str_trim() %>%
  currency_to_numeric()


# Get column names.
summary_column_names <- summary_string %>%
  get_column_names_string() %>%
  extract_column_names(.GlobalEnv$known_column_names)


# Get summary table.
summary_table <- summary_string %>%
  get_usage_lines() %>%
  get_usage_table(summary_column_names) %>%
  dplyr::mutate(
    across(
      .cols = everything(),
      .fns = ~dplyr::na_if(.x, " "))) %>%
  set_numeric_columns()


# Check summary totals.
summary_total_table <- summary_table %>%
  dplyr::pull(ncol(.)) %>%
  sum(na.rm = TRUE)

if (abs(summary_total_table - summary_total_report) > .GlobalEnv$accuracy_threshold) {
  
  warning_msg <- c(
    stringr::str_glue(
      "Summary total read from report and the sum of parsed summary lines don't match.
      Please double check for missing lines in the parsed summary data!"),
    x = stringr::str_glue("Summary total read from report: {summary_total_report}"),
    x = stringr::str_glue("Parsed summary lines total: {summary_total_table}"))
  
  rlang::warn(warning_msg)
  
} else {
  
  info_msg <- c(
    "Checked if summary total that was read from report\nmatches the total of parsed summary lines.",
    i = "All good!")
  
  rlang::inform(info_msg)
}


# Check summary total vs usage totals
usages_total <- all_usages_table %>%
  dplyr::pull(ncol(.)) %>%
  sum(na.rm = TRUE)

if (abs(usages_total - summary_total_report) > .GlobalEnv$accuracy_threshold) {
  
  warning_msg <- c(
    stringr::str_glue(
      "Summary total read from report and the sum of company usage lines don't match.
      Please double check for missing lines in the parsed usage data!"),
    x = stringr::str_glue("Summary total read from report: {summary_total_report}"),
    x = stringr::str_glue("Parsed usage lines total: {usages_total}"))
  
  rlang::warn(warning_msg)
  
} else {
  
  info_msg <- c(
    "Checked if summary total that was read from report\nmatches the total of parsed client usage lines.",
    i = "All good!")
  
  rlang::inform(info_msg)
}



##########
# Totals #
##########

totals_table <- grand_totals %>%
  tibble::enframe(
    name = "report_name",
    value = "price")



###############
# Export xlsx #
###############

# create xlsx worksheets
xlsx_workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(xlsx_workbook, sheetName = "TOTALS")
openxlsx::addWorksheet(xlsx_workbook, sheetName = "SUMMARY")
openxlsx::addWorksheet(xlsx_workbook, sheetName = "USAGE")


# write export data to worksheet
openxlsx::writeData(
  wb = xlsx_workbook,
  sheet = "TOTALS",
  x = totals_table)

openxlsx::writeData(
  wb = xlsx_workbook,
  sheet = "SUMMARY",
  x = summary_table)

openxlsx::writeData(
  wb = xlsx_workbook,
  sheet = "USAGE",
  x = all_usages_table)


# Auto-fit columns
openxlsx::setColWidths(
  wb = xlsx_workbook,
  sheet = "TOTALS",
  cols = 1:ncol(totals_table),
  widths = "auto")

openxlsx::setColWidths(
  wb = xlsx_workbook,
  sheet = "SUMMARY",
  cols = 1:ncol(summary_table),
  widths = "auto")

openxlsx::setColWidths(
  wb = xlsx_workbook,
  sheet = "USAGE",
  cols = 1:ncol(all_usages_table),
  widths = "auto")


# User input for overwriting
replace_file <- FALSE
if (file.exists(output_xlsx_path)) {
  
  replace_file <- ask_replace_file(output_xlsx_path)
  
  if (!replace_file) {
    
    info_msg <- c(
      "Script finished!",
      i = "User chose not to overwrite existing xlsx file.",
      i = "Parsed pdf data not exported.")
    
    rlang::inform(info_msg)
    abort_quietly()
  }
}


# export workbook
openxlsx::saveWorkbook(
  wb = xlsx_workbook, 
  file = output_xlsx_path,
  overwrite = replace_file)


# Fin
info_msg <- c(
  "Script finished!",
  i = "Parsed pdf data exported to xlsx.",
  i = stringr::str_glue("Output file path: {output_xlsx_path}\n\n"),
  "Take a minute to savour the absurdity of having to use a 1k-line regex parser script, because a global software vendor can't be bothered to issue machine-readable invoices. Sad.")

rlang::inform(info_msg)
