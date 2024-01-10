`%>%` = dplyr::`%>%`

get_locality_data <- function(
    df,
    norm_output_per = 100,
    norm_header = "None") {
  # Convert ward level data frame to constituency or locality level

  df <- df %>%
    dplyr::mutate("Constituency" = Name) %>%
    dplyr::select(-c("Name"))

  # Define data frame of BSol constituencies and their locality
  locality_list <- data.frame(
    Locality = c("East", "East", "West", "West",
                 "Central","Central","North", "North",
                 "South", "South", "Solihull", "Solihull"),
    Constituency = c("Hodge Hill", "Yardley","Ladywood", "Perry Barr",
                     "Selly Oak",  "Hall Green","Erdington",  "Sutton Coldfield",
                     "Northfield", "Edgbaston", "Solihull", "Meriden")
  )

  df <- df %>%
    dplyr::left_join(locality_list, by = "Constituency")
  #View(df)

  if (norm_header == "None") {
    out <- df %>%
      dplyr::group_by_("Locality") %>%
      dplyr::summarise(
        Value = sum(`Value`)
      )
  } else {
    out <- df %>%
      dplyr::group_by_("Locality") %>%
      dplyr::summarise(
        `Normed Value` = norm_output_per*sum(`Value`)/sum(`Norm`),
        Value = sum(`Value`),
        Norm = sum(`Norm`)
      ) %>%
      dplyr::select(c("Locality", "Value", "Norm", "Normed Value"))
  }
  return(out)
}

GP_weightings <- function(data,
                          GP_code_header,
                          value_header,
                          norm_header = "None",
                          weighting = "Ward",
                          norm_output_per = 100) {

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

  GP_data <- data %>%
    dplyr::select(all_of(GP_select_list)) %>%
    # Rename columns to make it easier to work with (changed back later)
    dplyr::rename(`Practice Code` = 1, `Value` = 2)

  # If there's a normalisation value, change the name for that too
  if (norm_header != "None"){
    GP_data = dplyr::rename(GP_data, `Norm` = 3)
  }

  #### Apply GP weights ####

  # Load GP weights file
  if (weighting == "Ward") {
    gpWeights <- GP_Ward
  } else if (weighting == "Constituency") {
    gpWeights <- GP_Constituency
  } else {
    stop("Error: Unexpected weighting")
  }

  # Check if all required GPs are there
  check_mask <- GP_data$`Practice Code` %in% gpWeights$`Practice Code`
  if (!all(check_mask)){
    print(paste(
      sum(!check_mask),
      "codes provided are missing from the conversion matrix.",
      sep = " ")
    )
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
      dplyr::select(c("Practice Code", sym(ward_i)))

    ward_i_weights= dplyr::rename(ward_i_weights,
                          ward_i_percs = 2)

    ward_i_counts <- ward_i_weights %>%
      dplyr::left_join(GP_data,
                by = "Practice Code")

    if (norm_header == "None") {
      ward_i_counts <- ward_i_counts %>%
        dplyr::mutate(
          `Area Value` = `Value`*ward_i_percs
        ) %>%
        dplyr::summarise(
          `Area Value` = sum(`Area Value`, na.rm=TRUE)
        )  %>%
        dplyr::mutate(
          Name = all_of(ward_i)
        )
    } else {
      ward_i_counts <- ward_i_counts %>%
        dplyr::mutate(
          `Area Value` = `Value`*ward_i_percs,
          `Area Norm` = `Norm`*ward_i_percs
        ) %>%
        dplyr::summarise(
          `Area Value` = sum(`Area Value`, na.rm=TRUE),
          `Area Norm` = sum(`Area Norm`, na.rm=TRUE),
          `Area Normed` = norm_output_per*`Area Value`/`Area Norm`
        ) %>%
        dplyr::mutate(
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

#' Title
#'
#' @param data data frame containing GP-level data
#' @param GP_code_header Column header for GP Code (e.g. M81062)
#' @param value_header Column header for variable of interest
#' @param to Area type to convert to (Ward/Constituency/Locality)
#' @param norm_header Column header for normalisation values
#' @param norm_output_per Normalisation level (e.g. per 100, per 1000)
#'
#' @return
#' @export
#'
#' @examples
convert_GP_data <- function(
    data,
    GP_code_header,
    value_header,
    to = "Ward",
    norm_header = "None",
    norm_output_per = 100
) {

  if (to %in% c("Ward","Constituency")) {
    weighting = to
  } else if (to == "Locality") {
    weighting = "Constituency"
  } else {
    stop("Error: 'to' must be one of: ['ward', 'constituency','locality']")
  }

  # Get ward/constituency values
  area_data <- GP_weightings(data,
                             GP_code_header,
                             value_header,
                             norm_header = norm_header,
                             norm_output_per = norm_output_per,
                             weighting = weighting)

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


