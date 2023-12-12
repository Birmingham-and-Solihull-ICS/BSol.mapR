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
  shape_path = paste(shape_file_path, "Constituency", sep = "")
  constituencies <- readOGR(
    shape_path,
    "Constituency",
    verbose = verbose
  )
  constituencies <- filter_shape(constituencies, area_name)

  # TODO: Remove this when switching to sf
  colnames(constituencies@data)[1] = "Constituency"
  
  # Add lines to map
  map <- map +
    tm_shape(constituencies) +
    tm_borders(col = "grey40", lwd = 1.5)
  
  if (const_names %in% c("None", "Yes", TRUE)){
    map <- map + tm_text(text = "Constituency", size = 0.8)
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
  shape_path = paste(shape_file_path, "Locality", sep = "")
  localities <- readOGR(
    shape_path,
    "Locality",
    verbose = FALSE
  )
  localities <- filter_shape(localities, area_name)
  
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

filter_shape <- function(
    shape, 
    area_name, 
    area_type = "") {
  # Cut the shape file down to the correct area
  if ((tolower(area) == "bsol") | area_type == "Postal District") {
    # Do nothing
  } else if (tolower(area) == "birmingham") {
    # Filter for Birmingham
    shape <- shape[shape@data$Area == "Birmingham",]
  } else if (tolower(area) == "solihull") {
    # Filter for Birmingham
    shape <- shape[shape@data$Area == "Solihull",]
  }
  return(shape)
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
  # Check for valid map type
  if (!(map_type %in% c("Locality", "Constituency", "Ward", 
                          "Postal District", "LSOA", "MSOA"))) {
    stop("Error: Unexpected map type")
  }
  
  # Check for valid area name
  if (!(area_name %in% c("BSol", "Birmingham", "Solihull"))) {
    stop("Error: Unexpected area type. Available options: 'BSol', 'Birmingham', 'Solihull'")
  }

  # Load base shape - to get correct map zoom
  base_path = paste(shape_file_path, "Locality", sep = "")
    base_shape <- readOGR(
    base_path,
    "Locality",
    verbose = verbose
  )
  base_shape <- filter_shape(base_shape, area_name)

  # Load shape data
  shape_path = paste(shape_file_path, map_type, sep = "")

  shape <- readOGR(
    shape_path,
    map_type,
    verbose = verbose
  )

  shape <- filter_shape(shape, area_type, map_type)
  

  
  # TODO: Remove this when switching to sf
  if (map_type == "Constituency") {
    colnames(shape@data)[1] = "Constituency"
  } else if (map_type == "Postal District") {
    colnames(shape@data)[1] = "Postal District"
  }
  
  # join data
  print(colnames(shape@data))
  print(colnames(area_data))
  #print(paste(abbreviate(map_type, 7)[[1]], "-", map_type))
  
  brum_merged <- merge(shape, 
                       area_data,
                       by = map_type)
  
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
