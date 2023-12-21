# GP-mapper

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# Load / install libraries
base_libs <- c("readxl", 
               "dplyr",
               "writexl")

for (lib in base_libs) {
  # print(paste("Loading:", lib))
  usePackage(lib)
}

weights_path_21 = paste(root_path, "../data/Brum_ward_info.xlsx", sep = "")
weights_path_23 = paste(root_path, "../data/BSol_ward_info.xlsx", sep = "")


get_locality_data <- function(
    df,
    norm_output_per = 100,
    norm_header = "None") {
  # Convert ward level data frame to constituency or locality level
  
  df <- df %>%
    mutate("Constituency" = Name) %>%
    select(-c("Name"))
  
  local_list <- read_excel(
    weights_path_21,
    sheet = "ward_list"
  ) %>%
    select("Constituency", 
           "Locality") %>%
    unique()
  
  df <- df %>%
    left_join(local_list, by = "Constituency") 
  #View(df)
  
  if (norm_header == "None") {
    out <- df %>%
      group_by_("Locality") %>%
      summarise(
        Value = sum(`Value`)
      )
  } else {
    out <- df %>%
      group_by_("Locality") %>%
      summarise(
        `Normed Value` = norm_output_per*sum(`Value`)/sum(`Norm`),
        Value = sum(`Value`),
        Norm = sum(`Norm`)
      ) %>%
      select(c("Locality", "Value", "Norm", "Normed Value"))
  }
  return(out)
}

GP_weightings <- function(file, 
                          GP_code_header,
                          value_header,
                          norm_header = "None",
                          weighting = "Ward",
                          norm_output_per = 100,
                          sheet = 1) {
  
  # make list of column headers to extract from GP data
  if (norm_header == "None") {
    GP_select_list = c(GP_code_header,
                       value_header)
    headers = c('Ward',
                'Area Value')
    ncol <- 2
  } else {
    GP_select_list = c(GP_code_header,
                       value_header, 
                       norm_header)
    headers = c('Ward',
                'Area Value', 
                'Area Norm',
                'Ward Percent')
    ncol <- 4
  }
  
  if (is.character(file)) {
    GP_data <- read_excel(file, sheet = sheet) %>%
      select(all_of(GP_select_list)) %>%
      # Rename columns to make it easier to work with (changed back later)
      rename(`Practice Code` = 1, `Value` = 2)
  } else if (is.data.frame(file)) {
    GP_data <- file %>%
      select(all_of(GP_select_list)) %>%
      # Rename columns to make it easier to work with (changed back later)
      rename(`Practice Code` = 1, `Value` = 2)
  } else {
    print("Error: Unrecognised input data type.")
  }
  
  # If there's a normalisation value, change the name for that too
  if (norm_header != "None"){
    GP_data = rename(GP_data, `Norm` = 3)
  }
  
  #### Apply GP weights ####
  
  # Load GP weights file
  if (weighting == "Ward") {
    gpWeights <- read_excel(weights_path_21, sheet = "ward_weighting")
  } else if (weighting == "Constituency") {
    gpWeights <- read_excel(weights_path_21, sheet = "const_weighting")
  } else {
    stop("Error: Unexpected weighting")
  }
  
  # Check if all required GPs are there
  check_mask <- GP_data$`Practice Code` %in% gpWeights$`Practice Code`
  if (!all(check_mask)){
    print("One or more GP codes provided are missing from the conversion matrix.")
    missingGPs <- GP_data$`Practice Code`[!check_mask]
    if (length(missingGPs) < 10) {
      print(missingGPs) 
    }
  }
  
  
  # get list of all wards
  allWardNames <- colnames(gpWeights)[2:length(colnames(gpWeights))]
  
  #create data frame with 0 rows and 3 columns
  areaCounts <- data.frame(matrix(ncol = ncol, nrow = 0))
  
  
  # Loop over all wards to sum contributions from each GP
  for (ward_i in allWardNames) {
    ward_i_weights <- gpWeights %>%
      select(c("Practice Code", sym(ward_i)))
    
    ward_i_weights=rename(ward_i_weights,
                          ward_i_percs = 2)
    
    ward_i_counts <- ward_i_weights %>%
      left_join(GP_data, 
                by = "Practice Code") 
    
    if (norm_header == "None") {
      ward_i_counts <- ward_i_counts %>%
        mutate(
          `Area Value` = `Value`*ward_i_percs
        ) %>%
        summarise(
          `Area Value` = sum(`Area Value`, na.rm=TRUE)
        )  %>%
        mutate(
          Name = all_of(ward_i)
        )
    } else {
      ward_i_counts <- ward_i_counts %>%
        mutate(
          `Area Value` = `Value`*ward_i_percs,
          `Area Norm` = `Norm`*ward_i_percs
        ) %>%
        summarise(
          `Area Value` = sum(`Area Value`, na.rm=TRUE),
          `Area Norm` = sum(`Area Norm`, na.rm=TRUE),
          `Area Normed` = norm_output_per*`Area Value`/`Area Norm`
        ) %>%
        mutate(
          Name = all_of(ward_i)
        )
    }
    
    areaCounts <- rbind(areaCounts, ward_i_counts)
  }
  areaCounts <- areaCounts %>%
    relocate(Name)
  
  colnames(areaCounts)[colnames(areaCounts) == 'Area Value'] <- "Value"
  if (norm_header!="None") {
    colnames(areaCounts)[colnames(areaCounts) == 'Area Norm'] <- "Norm"
    colnames(areaCounts)[colnames(areaCounts) == 'Area Normed'] <- "Normed Value"
  }
  
  return(areaCounts)
}

convert_GP_data <- function(
    file, 
    GP_code_header,
    value_header,
    to = "Ward",
    norm_header = "None",
    norm_output_per = 100,
    sheet = 1
) {
  
  if (to %in% c("Ward","Constituency")) {
    weighting = to
  } else if (to == "Locality") {
    weighting = "Constituency"
  } else {
    stop("Error: 'to' must be one of: ['ward', 'constituency','locality']")
  }
  
  # Get ward/constituency values
  area_data <- GP_weightings(file, 
                             GP_code_header,
                             value_header,
                             norm_header = norm_header,
                             norm_output_per = norm_output_per,
                             weighting = weighting,
                             sheet = sheet) 
  
  if (to == "Locality") {
    # aggregate to ward (do nothing), constituency or locality
    agged_data <- get_locality_data(
      area_data,
      norm_output_per = norm_output_per,
      norm_header = norm_header)
  } else {
    agged_data <- area_data
  }
  
  # Rename columns
  colnames(agged_data)[colnames(agged_data) == 'Name'] <- to
  colnames(agged_data)[colnames(agged_data) == 'Value'] <- value_header
  if (norm_header != "None") {
    colnames(agged_data)[colnames(agged_data) == 'Norm'] <- norm_header
    normed_str = paste(value_header,
                       "per",
                       norm_output_per,
                       norm_header)
    colnames(agged_data)[colnames(agged_data) == 'Normed Value'] <- normed_str
  }
  
  return(agged_data)
  
}

save_data <- function(
    data,
    save_path = "map_data.xlsx"
) {
  extention <- strsplit(x = (save_path), split = "\\.")[[1]][[2]]
  if (! (extention %in% c("xlsx", "csv") )) {
    stop("Error:file_type must be either 'xlsx' or 'csv'.")
  }
  
  if (extention == "csv"){
    write.csv(
      data, 
      save_path,
      row.names=FALSE)
  }
  
  if (extention == "xlsx") {
    write_xlsx(
      data, 
      save_path
    )
  }
}
