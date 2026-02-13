# Write an R script that does the following:
# list all possible data files (csv, xls, xlsx) in ../fedsdata
# read in each of these files and pull out waterbody name and date information
# create data frame with the file name, date, and waterbody
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

files <- list.files("../fedsdata", pattern = "\\.(csv|xls|xlsx)$", full.names = TRUE, 
                    recursive = TRUE)
#remove buoy data files from files object
files <- files[!grepl("buoy", files, ignore.case = TRUE)]
#remove files without other words in file name unrelated to rhodyponds
files <- files[!grepl("shubael", files, ignore.case = TRUE)]
files <- files[!grepl("hamblin", files, ignore.case = TRUE)]
files <- files[!grepl("phycotech", files, ignore.case = TRUE)]
files <- files[!grepl("flame", files, ignore.case = TRUE)]
files <- files[!grepl("exo", files, ignore.case = TRUE)]
files <- files[!grepl("sonde_", files, ignore.case = TRUE)]
files <- files[!grepl("hobo", files, ignore.case = TRUE)]

data_list <- lapply(files, function(file) {
  tryCatch({
  if (grepl("\\.csv$", file) & !grepl("exo2_logged", file)) {
    data <- read_csv(file, show_col_types = FALSE)
    
  } else if (grepl("\\.xls$|\\.xlsx$", file)) {
    data <- read_excel(file, show_col_types = FALSE)
  } else {
    return(NULL)
  }},
  error = NULL
  )
  
  data <- rename_all(data, tolower) # Convert column names to lowercase
  #cube
  if(grepl("cube", file)){
    # use stringr to extract the first four numbers of the first column in data
    id <- pull(data,1)
    years <- str_extract(id, "^\\d{4}") |>
      as.numeric()
    wb <- str_extract(id, str_extract(id, ".{3}$"))
    if(any(years < 2020 & grepl("m|w|y", tolower(wb)))){
      file.copy(file, "data/raw/microcystin/", overwrite = TRUE)
    }
  }
  
  #Field Data
  if(grepl("Field Data", file)){
    file.copy(file, "data/raw/", overwrite = TRUE)
  }
  
  #plate_reader
  if(grepl("plate_reader", file)){
    id <- pull(data,1) |> na.omit()
    years <- str_extract(id, "^\\d{4}") |>
      as.numeric()
    wb <- str_extract(id, str_extract(id, ".{3}$"))
    if(any(years < 2020 & grepl("m|w|y", tolower(wb)))){
      file.copy(file, "data/raw/microcystin/", overwrite = TRUE)
    }
  }
  #turner
  if(grepl("turner", file)){
    wb <- unique(data$waterbody)
    years <- unique(data$year)
    if(any(years < 2020 & grepl("mashapaug|warwick|yawgoo", tolower(wb)))){
      file.copy(file, "data/raw/microcystin/", overwrite = TRUE)
    }
  }
})


