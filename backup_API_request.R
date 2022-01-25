#!/usr/bin/env Rscript


########################################################################################################################################
##                                                                                                                                    ##
##  Script name: backup_API_request.R                                                                                                 ##
##  Purpose of script: pull data from N-able Backup & Recovery API                                                                    ##
##  API documentation: https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/home.htm   ##
##                                                                                                                                    ##
##  Author: Mart Roben                                                                                                                ##
##  Date Created: 24. Jan 2022                                                                                                        ##
##                                                                                                                                    ##
##  Copyright: BSD-3-Clause                                                                                                           ##
##  https://github.com/martroben/nable_scripts                                                                                        ##
##                                                                                                                                    ##
##  Contact: mart@altacom.eu                                                                                                          ##
##                                                                                                                                    ##
########################################################################################################################################


#################
# Load packages #
#################

if (!require("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  "httr",
  "tibble",
  "magrittr",
  "dplyr",
  "stringr",
  "purrr",
  "lubridate",
  "jsonlite",
  "argparser")



#########
# Input #
#########

# Global variables
API_url <<- "https://api.backup.management/jsonapi"
API_data_encoding <<- "UTF-8"

# Defaults added for testing purposes
# FOR SECURITY REASONS, DON'T HARDCODE THIS INFO INTO YOUR SCRIPT
username_default <- "APIuser@mail.com"
password_default <- "APIuser.password"
my_company_name_default <- "My Company"

p <- argparser::arg_parser("This script pulls data from N-able Backup & Recovery API.", hide.opts = TRUE)
p <- argparser::add_argument(p, "--user", help = "Username", default = username_default)
p <- argparser::add_argument(p, "--pass", help = "Password", default = password_default)
p <- argparser::add_argument(p, "--my_co", help = "Your company name in BU dashboard", default = my_company_name_default)

input_args <- argparser::parse_args(p)

username <- input_args$user
password <- input_args$pass
my_company_name <- input_args$my_co



##################
# API call codes #
##################

# Set as global variables
device_info_codes <<- c(
  
  # https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/API-column-codes.htm
  device_id = "I0",
  device_name = "I1",
  creation_date = "I4",
  status_code = "I7",
  partner = "I8",
  storage_location = "I11",
  os_version = "I16",
  computer_name = "I18",
  timezone_offset = "I24",
  os_type = "I32",
  anti_crypto_enabled = "I34",
  local_speedvault_enabled = "I35",
  local_speedvault_status = "I37",
  encryption_status = "I43",
  standby_image_enabled = "I51",
  standby_image_status = "I52",
  demo_company = "I72",
  active_data_sources = "I78",
  recovery_testing = "I80",
  physicality = "I81")


session_info_codes <<- c(
  
  # https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/API-column-codes.htm
  # Have to combine with data source to get a functional column code. eg. D1F0, D1F3, etc.
  last_session_status = "F0",
  last_session_selected_size = "F3",
  last_session_sent_size = "F5",
  last_successful_session_time = "F9",
  last_session_time = "F15",
  last_successful_session_status = "F16",
  last_completed_session_time = "F18",
  last_completed_session_status = "F17",
  session_duration = "F12")


data_sources_key <<- c(
  
  # https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/API-column-codes.htm
  "Files and Folders" = "D01",
  "System State" = "D02",
  "MsSql" = "D03",
  "M365 SharePoint" = "D05",
  "Network Shares" = "D06",
  "VSS System State" = "D07",
  "VMware VMs" = "D08",
  "Total" = "D09",
  "VSS MS SQL" = "D10",
  "VSS SharePoint" = "D11",
  "Hyper-V" = "D14",
  "MySql" = "D15",
  "Virtual Disaster Recovery" = "D16",
  "Bare Metal Restore" = "D17",
  "M365 Exchange" = "D19",
  "M365 OneDrive" = "D20")


session_status_key <<- c(
  
  # https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/API-column-codes.htm
  "in process" = 1,
  "failed" = 2,
  "aborted" = 3,
  "completed" = 5,
  "interrupted" = 6,
  "not started" = 7,
  "completed with errors" = 8,
  "in progress with faults" = 9,
  "over quota" = 10,
  "no selection" = 11,
  "restarted" = 12)



#############
# Functions #
#############

authorization_json <- function(my_company_name, username, password) {
  
  # https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/login.htm
  authorization_body <- list(
    jsonrpc = "2.0",
    method = "Login",
    params = list(
      partner = my_company_name,
      username = username,
      password = password),
    id = 1)
  
  jsonlite::toJSON(authorization_body, auto_unbox = TRUE)
}


partner_list_json <- function(my_company_id, access_visa) {
  
  # https://documentation.n-able.com/backup/userguide/documentation/Content/service-management/json-api/enumerate-customers.htm
  fetch_partners_body <- list(
    id = "jsonrpc",
    visa = access_visa,
    method = "EnumeratePartners",
    jsonrpc = "2.0",
    params = list(
      parentPartnerId = my_company_id,
      fields = c(0, 20),
      fetchRecursively = "true"))
  
  jsonlite::toJSON(fetch_partners_body, auto_unbox = TRUE, pretty = TRUE)
}


device_info_json <- function(partner_id, column_codes, access_visa, device_name = NA) {
  
  fetch_device_info_body <- list(
    id = "jsonrpc",
    visa = access_visa,
    method = "EnumerateAccountStatistics",
    jsonrpc = "2.0",
    params = list(
      query = list(
        PartnerId = partner_id,
        RecordsCount = 1e4,
        Columns = column_codes
      )
    )
  )
  
  if (!is.na(device_name)) {
    # if device ID is given, filter out info for that device only
    filter_string <- stringr::str_glue("I1 =~ '{device_name}'")
    fetch_device_info_body$params$query$Filter <- filter_string
  }
  
  jsonlite::toJSON(fetch_device_info_body, auto_unbox = TRUE, pretty = TRUE)
}


trim_extra_zero <- function(code) {
  
  # Turn string "I00" to "I0" (for all leading letters except "D")
  if (stringr::str_detect(code, "[A-CE-Za-ce-z]0[0-9]")) {
    
    trimmed_string <- code %>%
      stringr::str_match("([A-CE-Za-ce-z])0([0-9])") %>%
      magrittr::extract(c(2, 3)) %>% 
      purrr::reduce(stringr::str_c)
    
    return (trimmed_string)
    
  } else {return (code)}
}


remove_D_component <- function(code) {
  
  # Turns string "D1F0" to "F0"
  # Only "D1" without the second part is not modified
  if (stringr::str_detect(code, "D\\d+[A-Za-z]")) {
    
    trimmed_string <- code %>% 
      stringr::str_match("D\\d+(.*)") %>%
      magrittr::extract(2)
    
    return (trimmed_string)
    
  } else {return (code)}
}


safe_get_name <- function(code, key) {
  # Get name of value from a named vector (key).
  # If value is not present, return input as is
  
  if (code %in% key) {
    
    return (names(which(key == code)))
    
  } else {return (code)}
}


names_from_codes <- function(codes, key = c(.GlobalEnv$device_info_codes, .GlobalEnv$session_info_codes)) {
  
  codes %>%
    purrr::map_chr(remove_D_component) %>%
    purrr::map_chr(trim_extra_zero) %>%
    purrr::map_chr(~safe_get_name(.x, key))
}


get_partner_device_info <- function(partner_id, query_fields, access_visa, device_name = NA) {
  
  # add device name and device id fields if not included
  if (!("I1" %in% query_fields)) query_fields <- c("I1", query_fields)
  if (!("I0" %in% query_fields)) query_fields <- c("I0", query_fields)
  
  device_info_reply_raw <- httr::POST(
    url = .GlobalEnv$API_url,
    body = device_info_json(partner_id, query_fields, access_visa, device_name = device_name))
  
  device_info_reply_raw %>%
    httr::content(as = "text", encoding = .GlobalEnv$API_data_encoding) %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE) %>%
    purrr::pluck("result", "result") %>%
    purrr::map(~purrr::pluck(.x, "Settings")) %>%
    purrr::map_dfr(unlist) %>%
    dplyr::rename_with(~names_from_codes(.x)) %>%
    dplyr::mutate(partner_id = partner_id)
}


split_data_sources_string <- function(data_sources_string) {
  
  # split string "D01D14" to vector "D01", "D14"
  data_sources_string %>%
    stringr::str_match_all("D\\d+") %>%
    purrr::map(~c(.x))
}


get_session_info <- function(..., query_fields) {
  
  device_info_row <- tibble::tibble(...)
  
  get_partner_device_info(
    partner_id = device_info_row$partner_id,
    query_fields = stringr::str_c(device_info_row$active_data_sources, query_fields),
    access_visa = access_visa,
    device_name = device_info_row$device_name
    ) %>%
    purrr::when(
      "active_data_sources" %in% colnames(device_info_row) ~ dplyr::mutate(., active_data_sources = device_info_row$active_data_sources),
       ~ .)
}



#############
# Execution #
#############

# Authorization
authorization_reply_raw <- httr::POST(
  url = .GlobalEnv$API_url,
  body = authorization_json(my_company_name, username, password))

authorization <- authorization_reply_raw %>%
  httr::content(as = "text", encoding = .GlobalEnv$API_data_encoding) %>%
  jsonlite::fromJSON()

access_visa <- authorization$visa
my_company_id <- authorization %>%
  purrr::pluck("result", "result", "PartnerId")


# Get partner list
partners_reply_raw <- httr::POST(
  url = .GlobalEnv$API_url,
  body = partner_list_json(my_company_id, access_visa))

partners <- partners_reply_raw %>%
  httr::content(as = "text", encoding = .GlobalEnv$API_data_encoding) %>%
  jsonlite::fromJSON() %>%
  purrr::pluck("result", "result") %>%
  dplyr::select(Name, Id, CreationTime) %>%
  dplyr::mutate(CreationTime = lubridate::as_datetime(CreationTime))


# Get device info
device_query_variables <- c(
  "partner",
  "device_name",
  "active_data_sources")

device_query_codes <- .GlobalEnv$device_info_codes %>%
  magrittr::extract(device_query_variables) %>% 
  unname()

all_devices <- partners$Id %>%
  purrr::map_dfr(
    ~get_partner_device_info(
      partner_id = .x,
      device_query_codes,
      access_visa))


# Get session info
session_query_variables <- c(
  "last_session_time",
  "last_session_status",
  "last_session_selected_size",
  "last_successful_session_time",
  "last_successful_session_status",
  "last_completed_session_time",
  "last_completed_session_status",
  "session_duration",
  "last_session_sent_size")

session_query_codes <- .GlobalEnv$session_info_codes %>%
  magrittr::extract(session_query_variables) %>% 
  unname()

all_data_sources <- all_devices %>%
  dplyr::mutate(active_data_sources = split_data_sources_string(active_data_sources)) %>%
  purrr::pmap_dfr(~tibble::tibble(...))

all_session_info <- all_data_sources %>%
  purrr::pmap_dfr(get_session_info, query_fields = session_query_codes)


# format for presentation
time_variables <- c(
  "last_session_time",
  "last_completed_session_time",
  "last_successful_session_time")

result_code_variables <- c(
  "last_session_status",
  "last_completed_session_status",
  "last_successful_session_status")

amount_of_data_variables <- c(
  "last_session_sent_size",
  "last_session_selected_size")

variables_to_keep <- c(
  "partner_id",
  "device_id",
  "partner",
  "device_name",
  "data_source",
  "last_session_time",
  "session_duration_minutes",
  "last_session_status",
  "last_session_selected_size_GB",
  "last_session_sent_size_GB",
  "last_completed_session_time",
  "last_completed_session_status",
  "last_successful_session_time",
  "last_successful_session_status")


session_info_formatted <- all_session_info %>%
  dplyr::inner_join(
    all_data_sources,
    by = c(
      "device_id",
      "device_name",
      "partner_id",
      "active_data_sources")
    ) %>%
  dplyr::mutate(
    across(all_of(time_variables), ~as.numeric(.x) %>% lubridate::as_datetime()),
    across(all_of(amount_of_data_variables), ~as.numeric(.x) %>% magrittr::divide_by(2^10 * 2^10 * 2^10) %>% round(2)),
    across(all_of(result_code_variables), ~names_from_codes(.x, key = .GlobalEnv$session_status_key)),
    active_data_sources = names_from_codes(active_data_sources, key = .GlobalEnv$data_sources_key),
    session_duration = session_duration %>% as.numeric() %>% magrittr::divide_by(60) %>% round(2)
    ) %>%
  dplyr::rename(
    last_session_selected_size_GB = last_session_selected_size,
    last_session_sent_size_GB = last_session_sent_size,
    session_duration_minutes = session_duration,
    data_source = active_data_sources
    ) %>%
  dplyr::select(all_of(variables_to_keep))



###############
# Return data #
###############

session_info_formatted
