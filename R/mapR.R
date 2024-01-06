# GP-to-ward map generator
#
# This module uses GP data to estimate the ward-level distribution of health
# outcomes. This data can then be plotted as a heat map.
#
# * Based on 2021 ward weights
# TODO: Tidy everything
# TODO: Update Postal District shape file to include all B, WS and CV codes

options(warn=-1)

#root_path = "C:/Users/TMPCDDES/OneDrive - Birmingham City Council/Documents/Main work/MiscCode/BSol-mapR/R/"
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
               "writexl",
               "sp",
               "tmap")

#options("rgdal_show_exportToProj4_warnings"="none")

for (lib in base_libs) {
  # print(paste("Loading:", lib))
  usePackage(lib)
}

check_type_and_area <- function(map_type, area_name) {
  # Check for valid map type
  if (!(map_type %in% c("Locality", "Constituency", "Ward", "Postal District",
                        "Postal Sector", "LSOA11", "MSOA11", "LSOA21", "MSOA21"))) {
    stop(paste("Error: Unexpected map type. Given `", map_type,"`", sep = ""))
  }

  # Check for valid area name
  if (!(area_name %in% c("BSol", "Birmingham", "Solihull"))) {
    stop(
      paste(
        "Error: Unexpected area type. Available options: 'BSol', 'Birmingham', 'Solihull'\nGiven: `",
        area_name,"`", sep = "")
    )
  }
}

load_shape_file <- function(map_type) {
  # Load lazy loaded shape file
  shape <- switch(
    map_type,
    "Constituency" = Constituency,
    "Locality" = Locality,
    "LSOA11" = LSOA11,
    "LSOA21" = LSOA21,
    "MSOA11" = MSOA11,
    "MSOA21" = MSOA21,
    "Postal District" = `Postal District`,
    "Postal Sector" = `Postal Sector`,
    "Ward" = Ward,
    stop("Unknown map type.")
  )

  return(shape)
}

add_const_lines <- function(
    map,
    area_name = "Birmingham",
    const_names = "None",
    verbose = FALSE
) {

  # TODO: Fix hard coded file path
  constituencies <- load_shape_file("Constituency")
  constituencies <- filter_shape(constituencies, area_name, area_type = "Constituency")

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

  localities <- load_shape_file("Locality")
  localities <- filter_shape(localities, area_name, area_type = "Locality")

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
    area_type
) {

  # Cut the shape file down to the correct area
  if ((tolower(area_name) == "bsol")) {
    # Do nothing
  } else if (tolower(area_name) == "birmingham") {
    # Filter for Birmingham
    shape <- shape[shape@data$Area == "Birmingham",]
  } else if (tolower(area_name) == "solihull") {
    # Filter for Birmingham
    shape <- shape[shape@data$Area == "Solihull",]
  }

  return(shape)
}

remove_nas <- function(
    shape,
    value_header
) {
  # filter shape down to areas we have data for
  mask <- !is.na(shape@data[[value_header]])
  shape <- shape[mask,]

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
    area_name = "Birmingham",
    map_type = "Ward",
    pallet = "Blues",
    style = "pretty",
    breaks = NULL,
    verbose = FALSE
) {
  # Check for valid map type and area name
  check_type_and_area(map_type, area_name)

  # Load base shape - to get correct map zoom
  base_shape <- load_shape_file("Locality")
  base_shape <- filter_shape(base_shape, area_name, map_type)

  # Load shape data
  shape_path = paste(shape_file_path, map_type, sep = "")

  shape <- load_shape_file(map_type)

  # TODO: Remove this when switching to sf
  if (map_type == "Constituency") {
    colnames(shape@data)[1] = "Constituency"
  } else if (map_type == "Postal District") {
    colnames(shape@data)[1] = "Postal District"
  } else if (map_type == "Postal Sector") {
    colnames(shape@data)[1] = "Postal Sector"
  }

  # join data
  shape@data <- shape@data %>%
    left_join(area_data, by = map_type)

  if (map_type %in% c("Postal District", "Postal Sector")) {
    shape <- remove_nas(shape, value_header)
  } else {
    shape <- filter_shape(shape, area_name, map_type)
  }

  # Turn off borders for LSOA maps
  if (map_type %in% c("LSOA11", "LSOA21")) {
    alpha = 0
  } else {
    alpha = 1
  }

  #### plot map ####
  map <- tm_shape(base_shape) +
    # Invisible base layer to fix map zoom
    tm_borders(lwd = 0) +
    tm_shape(shape) +
    tm_fill(
      value_header,
      title = map_title,
      palette = pallet,
      style=style,
      breaks = breaks
    ) +
    tm_borders(col = "grey80", lwd = 0.4, alpha = alpha) +
    tm_layout(legend.position = c("LEFT", "TOP"),
              legend.width = 0.5,
              legend.height = 0.5,
              legend.frame = FALSE,
              inner.margins = 0.08)

  return(map)
}


plot_empty_map <- function(
    map_title = "",
    area_name = "Birmingham",
    map_type = "Ward",
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
  # Check for valid map type and area name
  check_type_and_area(map_type, area_name)

  # Load base shape - to get correct map zoom
  base_shape <- load_shape_file("Locality")

  base_shape <- filter_shape(base_shape, area_name, map_type)


  # Load shape data
  shape_path = paste(shape_file_path, map_type, sep = "")

  shape <- load_shape_file(map_type)

  if (!(map_type %in% c("Postal District", "Postal Sector"))) {
    shape <- filter_shape(shape, area_name, map_type)
  }

  # TODO: Remove this when switching to sf
  if (map_type == "Constituency") {
    colnames(shape@data)[1] = "Constituency"
  } else if (map_type == "Postal District") {
    colnames(shape@data)[1] = "Postal District"
  }

  #### plot map ####
  map <- tm_shape(base_shape) +
    # Invisible base layer to fix map zoom
    tm_borders(lwd = 0) +
    tm_shape(shape) +
    tm_borders(col = "grey80", lwd = 0.65) +
    tm_layout(legend.position = c("LEFT", "TOP"),
              legend.width = 0.5,
              legend.height = 0.5,
              legend.frame = FALSE,
              inner.margins = 0.08)

  # Add constituency lines
  if (
    const_lines %in% c("Yes", TRUE) |
    (map_type %in% c("Ward", "Constituency", "Postal District", "Postal Sector",
                     "LSOA11", "MSOA11", "LSOA21", "MSOA21") &
     locality_lines == "None" &
     !(const_lines %in% c("No", FALSE))
    )
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

  return(map)
}

plot_map <- function(
    data,
    value_header,
    area_name = "Birmingham",
    map_type = "Ward",
    map_title = "",
    pallet = "Blues",
    style = "pretty",
    breaks = NULL,
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

  tmap_options(show.messages = verbose)

  map <- plot_base_map(
    data,
    value_header,
    area_name = area_name,
    map_title = map_title,
    map_type = map_type,
    pallet = pallet,
    style = style,
    breaks = breaks,
    verbose = verbose
  )

  # Add constituency lines
  if (
    const_lines %in% c("Yes", TRUE) |
    (map_type %in% c("Ward", "Constituency", "Postal District", "Postal Sector",
                     "LSOA11", "MSOA11", "LSOA21", "MSOA21") &
     locality_lines == "None" &
     !(const_lines %in% c("No", FALSE))
    )
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


  return(map)
}

add_points <- function(
    map,
    points_data,
    size = 0.1,
    color = "orange",
    palette = "Dark2"
) {
  # If colnames don't include LONG and LAT - Pull from postcode
  if (
    ! ("LONG" %in% colnames(points_data) &
       "LAT" %in% colnames(points_data))
  ) {
    # No LONG and LAT so check that Postcode exists
    if (!"Postcode" %in% colnames(points_data)) {
      stop("Error: Expected column named `Postcode`")
    }
    else {
      # Load postcode look-up and join
      points_data <- points_data %>%
        left_join(
          # Lazy loaded postcode data
          WM_Postcodes
        )
      # Check for postcodes without coords
      missing_coords <- points_data$Postcode[is.na(points_data$LONG)]
      if (length(missing_coords) > 0) {
        print("The following postcodes could not be found.")
        print(missing_coords)
      }

    }
  }

  # Create new shape with high street points
  point_locs <- SpatialPointsDataFrame(
    data.frame(points_data$LONG, points_data$LAT),
    points_data,
    proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Update coordinate system
  point_locs <- spTransform(point_locs, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")

  map <- map +
    tm_shape(point_locs) +
    tm_dots(size = size,
            col = color,
            palette = palette)

  return(map)
}

save_map <- function(
    map,
    save_name = "new_map.png",
    height = 5,
    weight = 5
){

  tmap_save(map,
            filename = save_name,
            height = height,
            width = weight)

  print(paste("Map saved to:", save_name))
}

