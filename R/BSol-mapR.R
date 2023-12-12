# GP-to-ward map generator
#
# This module uses GP data to estimate the ward-level distribution of health
# outcomes. This data can then be plotted as a heat map.
#
# * Based on 2021 ward weights 
# TODO: Tidy everything
# TODO: Calculate new ward conversion matrix

options(warn=-1)

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

shape_file_path = "../data/Shape Files/"

# Load / install libraries
base_libs <- c("readxl", 
               "dplyr",
               "writexl")

for (lib in base_libs) {
  # print(paste("Loading:", lib))
  usePackage(lib)
}

weights_path_21 = "../data/Brum_ward_info.xlsx"
weights_path_23 = "../data/BSol_ward_info.xlsx"


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


add_const_lines <- function(
    map,
    area_name = "Birmingham",
    const_names = "None",
    verbose = FALSE
) {
  
  # TODO: Fix hard coded file path
  shape_path = paste(shape_file_path, area_name, "/","constituencies", sep = "")
  constituencies <- readOGR(
    paste(shape_path, sep = ""),
    "constituencies",
    verbose = verbose
  )
  
  # Remove "Birmingham" from constituency names
  constituencies$name = gsub("Birmingham, ", "",
                             x = constituencies$PCON22NM)
  # Add lines to map
  map <- map +
    tm_shape(constituencies) +
    tm_borders(col = "grey40", lwd = 1.5)
  
  if (const_names %in% c("None", "Yes", TRUE)){
    map <- map + tm_text(text = "name", size = 0.6)
  }
  
  return(map)
}

add_locality_lines <- function(
    map,
    area_name = "Birmingham",
    locality_names = "None",
    verbose = FALSE
) {
  #TODO: Fix hard coded file path
  shape_path = paste(shape_file_path, area_name, "/","localities", sep = "")
  localities <- readOGR(
    paste(shape_path, sep = ""),
    "localities",
    verbose = FALSE
  )
  
  map <- map +
    tm_shape(localities) +
    tm_borders(col = "grey40", lwd = 1.5)
  
  if (locality_names %in% c("None", "Yes", TRUE)){
    map <- map + tm_text(text = "Locality", size = 0.6)
  }
  
  return(map)
}


add_compass <- function(map) {
  map <- map + 
    tm_compass(type = "8star", size = 4,
               position = c("RIGHT", "bottom"))
  return(map)
}

add_credits<- function(map, credits, credits_size) {
  map <- map + 
    tm_credits(credits, size = credits_size,
               position = c(0, 0))
  return(map)
}

plot_base_map <- function(
    area_data,
    value_header,
    map_title,
    save_name,
    area_name = "Birmingham",
    map_type = "Ward",
    pallet = "Blues",
    verbose = FALSE
) {
  
  if (map_type == "Ward"){
    shape_type = "wards"
    shape_header = "ward_name"
  } else if (map_type == "Constituency"){
    shape_type = "constituencies"
    shape_header = "const_name"
  } else if (map_type == "Locality"){
    shape_type = "localities"
    shape_header = "Locality"
  } else {
    stop("Error: Unexpected map type")
  }
  
  # Check for valid area name
  if (!(area_name %in% c("BSol", "Birmingham", "Solihull"))) {
    stop("Error: Unexpected area type. Available options: 'BSol', 'Birmingham', 'Solihull'")
  }
  
  # TODO: Fix hard coded file path
  shape_path = paste(shape_file_path, area_name, "/",shape_type, sep = "")
  print(shape_path)
  # Load ward shape data
  shape <- readOGR(
    shape_path,
    shape_type,
    verbose = verbose
  )
  if (map_type == "Constituency"){
    shape$const_name = gsub("Birmingham, ", "",
                            x = shape$PCON22NM)
  }
  
  # join ward data
  brum_merged <- merge(shape, 
                       area_data,
                       by.x = shape_header,
                       by.y = colnames(area_data)[[1]])
  
  brum_merged@data[is.na(brum_merged@data)] <- 0
  
  #### plot map ####
  map <- tm_shape(brum_merged) +
    tm_fill(value_header,
            title = map_title,
            palette = pallet,
            style="pretty") +
    tm_borders(col = "grey80", lwd = 0.65) +
    tm_layout(legend.position = c("LEFT", "TOP"),
              legend.width = 0.5,
              legend.height = 0.5,
              legend.frame = FALSE,
              inner.margins = 0.08) 
  
  return(map)
}

plot_map <- function(
    data,
    value_header,
    area_name = "Birmingham",
    map_type = "Ward",
    save_name = "new_map.png",
    map_title = "",
    pallet = "Blues",
    const_lines = "None",
    const_names = "None",
    locality_lines = "None",
    locality_names = "None",
    compass = TRUE,
    credits = "Contains OS data \u00A9 Crown copyright and database right 2020. Source:
Office for National Statistics licensed under the Open Government Licence v.3.0.",
    credits_size = 0.6,
    verbose = FALSE
) {
  
  options("rgdal_show_exportToProj4_warnings"="none")
  map_libs <- c("rgdal", "tmap")
  for (lib in map_libs) {
    usePackage(lib)
  }
  tmap_options(show.messages = verbose)
  
  map <- plot_base_map(
    data,
    value_header,
    area_name = area_name,
    map_title = map_title,
    save_name = save_name,
    map_type = map_type,
    pallet = pallet,
    verbose = verbose
  )
  
  # Add constituency lines
  if ((const_lines %in% c("Yes", TRUE)) |
      ((map_type == "Ward") &
       (locality_lines == "None") & 
       !(const_lines %in% c("No", FALSE))) |
      ((map_type == "Constituency") & 
       !(const_lines %in% c("No", FALSE)) & 
       !(locality_lines %in% c("Yes", TRUE)))
  ) {
    map <- add_const_lines(map, 
                           area_name = area_name,
                           const_names = const_names, 
                           verbose = verbose)
  }
  
  if (locality_lines %in% c("Yes", TRUE) |
      (map_type == "Locality")) {
    map <- add_locality_lines(map, 
                              area_name = area_name,
                              locality_names = locality_names, 
                              verbose = verbose)
  }
  
  # Add compass
  if (compass %in% c("Yes", TRUE)){
    map <- add_compass(map)
  }
  
  if (credits != "None") {
    map <- add_credits(map, credits, 
                       credits_size = credits_size)
  }
  
  tmap_save(map,
            filename = save_name, 
            height = 5, 
            width = 5)
  
  print(paste("Map saved to:", save_name))
  return(map)
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
