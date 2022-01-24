#!/usr/bin/env Rscript


#######################################################################################################
##                                                                                                   ##
##  Script name: rmm_usage_script.R                                                                  ##
##  Purpose of script: format and summarize data from N-able Remote Monitoring & Management          ##
##                     monthly usage report                                                          ##
##  Author: Mart Roben                                                                               ##
##  Date Created: 29. Dec 2021                                                                       ##
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

# Cleaning the environment (to avoid conflicts with objects from previous session)
rm(list = ls(all.names = TRUE))

# Installing & loading necessary packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load("magrittr",
               "dplyr",
               "stringr",
               "purrr",
               "openxlsx",
               "readr",
               "argparser",
               "rlang")



#########
# Input #
#########

# Standard node price without volume & other discounts.
# Used to determine active nodes in usage report
active_node_standard_price_default <- 2.99

RMM_usage_csv_default <- "C:/Temp/rmm_usage_report.csv"
export_xlsx_default <- "C:/Temp/rmm_usage.xlsx"

p <- argparser::arg_parser("This script formats and summarizes the N-able Remote Monitoring & Management monthly usage report.", hide.opts = TRUE)
p <- argparser::add_argument(p, "--usage", help = "Usage report csv full path.", default = RMM_usage_csv_default)
p <- argparser::add_argument(p, "--out_xlsx", help = "Output xlsx full path.", default = export_xlsx_default)
p <- argparser::add_argument(p, "--node_price", help = "Full standard node price.", default = active_node_standard_price_default)

input_args <- argparser::parse_args(p)

active_node_standard_price <- input_args$node_price
RMM_usage_csv_path <- input_args$usage
export_xlsx_path <- input_args$out_xlsx



#############
# Functions #
#############

fix_strings <- function(string_vector, replacements) {
  
  fix_string <- function(string, fixes) {
    
    for (i in 1:nrow(fixes)) {
      if (!is.na(string) && stringr::str_detect(string, fixes$pattern[i])) {
        string <- stringr::str_replace_all(string, fixes$pattern[i], fixes$replacement[i])
      }
    }
    return (string)
  }
  return (purrr::map_chr(string_vector, fix_string, fixes = replacements))
}


mark_node_types <- function(usage_report, standard_node_price) {
  
  active_node_regex_string <- "(?i)(Workstation|Server|Network).* monitoring"
  
  usage_report %>%
    dplyr::mutate(
      active_node = stringr::str_detect(`Charge Description`, active_node_regex_string) &
                    `Remote Monitoring Management` == standard_node_price,
      asset_tracking = stringr::str_detect(`Charge Description`, active_node_regex_string) &
                    `Remote Monitoring Management` == 0)
}


# Get true node price with applicable volume discount
# 8%, 16.4%, 33.4% and 55.5% discounts at different usage levels 
get_true_node_price <- function(n_act_nodes, std_node_price) {

  n_act_nodes %>%
    purrr::when(
      . < 51 ~ std_node_price,
      . < 101 ~ std_node_price * (1-0.08),
      . < 201 ~ std_node_price * (1-0.164),
      . < 2001 ~ std_node_price * (1-0.334),
      ~ std_node_price * (1-0.555))
}


change_node_price <- function(usage_report, old_price, new_price) {
  
  new_price_usage_report <- usage_report %>% 
    dplyr::mutate(
      change_price = `Remote Monitoring Management` == old_price,
      `Remote Monitoring Management` = dplyr::case_when(
        change_price ~ new_price,
        TRUE ~ `Remote Monitoring Management`),
      Subtotal = dplyr::case_when(
        change_price ~ Subtotal - old_price + new_price,
        TRUE ~ Subtotal)) %>%
    dplyr::filter(!is.na(Client) | !stringr::str_detect(`Charge Description`, "(?i)discount"))
  
  old_total <- sum(usage_report$Subtotal, na.rm = TRUE)
  new_total <- sum(new_price_usage_report$Subtotal, na.rm = TRUE)
  
  if (round(old_total, 2) != round(new_total, 2)) {
    warning_msg <- stringr::str_glue("Old total and new total didn't match, while trying to adjust prices!
                                      Old total: {old_total} EUR
                                      New total: {new_total} EUR
                                      Volume discounts were not applied,
                                      please review and adjust prices manually!")
    rlang::warn(warning_msg)
    return (usage_report)
  }
  rlang::inform("Volume discounts applied successfully!")
  return (dplyr::select(new_price_usage_report, -change_price))
}


export_xlsx_RMM <- function(nodes, globals, totals, export_path) {
  
  xlsx_workbook <- openxlsx::createWorkbook()
  addWorksheet(xlsx_workbook, sheetName = "Nodes")
  addWorksheet(xlsx_workbook, sheetName = "Totals")
  
  # write export data to worksheet
  openxlsx::writeData(wb = xlsx_workbook,
                      sheet = "Nodes",
                      x = nodes)
  
  openxlsx::writeData(wb = xlsx_workbook,
                      sheet = "Totals",
                      x = totals)
  
  openxlsx::writeData(wb = xlsx_workbook,
                      sheet = "Totals",
                      startRow = nrow(totals) + 5,
                      x = "GLOBAL FEES")
  
  openxlsx::writeData(wb = xlsx_workbook,
                      sheet = "Totals",
                      startRow = nrow(totals) + 6,
                      x = globals)
  
  openxlsx::setColWidths(wb = xlsx_workbook,
                         sheet = "Nodes",
                         cols = 1:ncol(nodes),
                         widths = "auto")
  
  openxlsx::setColWidths(wb = xlsx_workbook,
                         sheet = "Totals",
                         cols = 1:ncol(totals),
                         widths = "auto")
  
  openxlsx::saveWorkbook(wb = xlsx_workbook, 
                         file = export_path,
                         overwrite = TRUE)
}



#############
# Execution #
#############

rlang::inform("Loading csv...")
RMM_usedata_raw <- readr::read_csv(file = RMM_usage_csv_path,
                                    col_types = readr::cols(),
                                    locale = readr::locale(encoding = "latin1"))


rlang::inform("Formatting report data...")
# Corrupted characters and their replacements:
fix_string_replacements <- tibble::tribble(
  ~pattern, ~replacement,
  "Ã¤",     "ä",
  "\\t",    " ")

RMM_usedata_formatted <- RMM_usedata_raw %>%
  dplyr::mutate(dplyr::across(where(is.character), ~fix_strings(.x, fix_string_replacements))) %>%
  mark_node_types(active_node_standard_price)


rlang::inform("Trying to apply correct volume discount to node prices...")
n_active_nodes <- sum(RMM_usedata_formatted$active_node)
true_node_price <- get_true_node_price(n_active_nodes, active_node_standard_price)
RMM_usedata_correct_prices <- RMM_usedata_formatted %>%
  change_node_price(active_node_standard_price, true_node_price)


rlang::inform("Calculating totals...")
RMM_nodes <- RMM_usedata_correct_prices %>%
  dplyr::filter(!is.na(Client))

RMM_global_fees <- RMM_usedata_correct_prices %>%
  dplyr::filter(is.na(Client)) %>%
  dplyr::select(where(~ (is.character(.x) && !all(is.na(.x))) | (is.numeric(.x) && sum(.x, na.rm = TRUE) > 0)))

RMM_totals <- RMM_nodes %>%
  dplyr::group_by(Client, Site) %>%
  dplyr::summarise(`Active Nodes` = sum(active_node),
                   `Asset Tracking Nodes` = sum(asset_tracking),
                   `Active Node Price` = true_node_price,
                   dplyr::across(where(is.numeric), ~sum(., na.rm = TRUE)),
                   .groups = "drop")


rlang::inform("Checking totals...")
input_csv_total <- sum(RMM_usedata_raw$Subtotal, na.rm = TRUE)
output_report_total <- (sum(RMM_totals$Subtotal, na.rm = TRUE) + sum(RMM_global_fees$Subtotal, na.rm = TRUE))

if (abs(input_csv_total - output_report_total) > 1) {
  
  input_file <- basename(RMM_usage_csv_path)
  output_file <- basename(export_xlsx_path)
  warning_msg <- stringr::str_glue("Input file '{input_file}' total doesn't match output file '{output_file}' total:
                                    input total: {input_csv_total} EUR
                                    output total: {output_report_total} EUR
                                    There is probably some error in the script.
                                    Please double-check the results manually!")
  rlang::warn(warning_msg)
}


rlang::inform("Exporting xlsx...")
export_xlsx_RMM(RMM_nodes, RMM_global_fees, RMM_totals, export_xlsx_path)


rlang::inform(stringr::str_c("Output xlsx created at ", export_xlsx_path))

