# GP-to-ward map generator
#
# This module uses GP data to estimate the ward-level distribution of health
# outcomes. This data can then be plotted as a heat map.
#
# * Based on 2021 ward weights 
# TODO: Tidy everything
# TODO: Calculate new ward conversion matrix

options(warn=-1)

root_path = ""
shape_file_path = paste(root_path, "../data/Shape Files/", sep = "")

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
    map <- map + tm_text(text = "name", size = 0.8)
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
  } else if (map_type == "Postal District"){
    shape_type = "Postal District"
    shape_header = "PostDist"
  }else {
    stop("Error: Unexpected map type")
  }
  
  # Check for valid area name
  if (!(area_name %in% c("BSol", "Birmingham", "Solihull"))) {
    stop("Error: Unexpected area type. Available options: 'BSol', 'Birmingham', 'Solihull'")
  }
  
  # TODO: Fix hard coded file path
  if (shape_type == "Postal District") {
    shape_path = paste(shape_file_path, shape_type, sep = "")
  } else {
    shape_path = paste(shape_file_path, area_name, "/",shape_type, sep = "")
  }

  # Load base shape - to get correct map zoom
  base_path = paste(shape_file_path, area_name, "/localities", sep = "")
  base_shape <- readOGR(
    base_path,
    "localities",
    verbose = verbose
  )

  # Load shape data
  shape <- readOGR(
    shape_path,
    shape_type,
    verbose = verbose
  )

  
  if (map_type == "Constituency"){
    shape$const_name = gsub("Birmingham, ", "",
                            x = shape$PCON22NM)
  }
  
  # join data
  brum_merged <- merge(shape, 
                       area_data,
                       by.x = shape_header,
                       by.y = colnames(area_data)[[1]])
  
  brum_merged@data[is.na(brum_merged@data)] <- 0
  
  #### plot map ####
  map <- tm_shape(base_shape) +
    # Invisible base layer to fix map zoom
    tm_borders(lwd = 0) + 
    tm_shape(brum_merged) +
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
       !(locality_lines %in% c("Yes", TRUE))) |
      ((map_type == "Postal District")& 
       !(const_lines %in% c("No", FALSE)))
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
