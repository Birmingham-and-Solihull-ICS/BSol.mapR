#' Load data file
#'
#' @param file_name Path to data file (.xls, .xlsx or .csv)
#' @param sheet Sheet name/number (only for xls/xlsx files)
#'
#' @return data frame containing loaded data
#' @export

load_data_file <- function(
    file_path = "output_data.xlsx",
    sheet = 1
) {

  split_name <- strsplit(x = file_path, split = "\\.")[[1]]
  extention <- split_name[length(split_name)]

  if (extention == "csv"){
    data <- read.csv(
      file_path,
      row.names=FALSE)
  } else if (extention %in% c("xls", "xlsx")) {
    data <- readxl::read_excel(
      file_path,
      sheet = sheet
    )
  } else {
    stop("Error: file_type must be either 'xlsx', 'xls', 'csv'.")
  }

  return(data)

}

#' Save data file
#'
#' @param data Data frame to be saved
#' @param save_path Save file name and path
#'
#' @export
save_data_file <- function(
    data,
    save_path = "output_data.xlsx"
) {

  split_name <- strsplit(x = save_path, split = "\\.")[[1]]
  extention <- split_name[length(split_name)]

  if (extention == "csv"){

    write.csv(
      data,
      save_path,
      row.names=FALSE)

  } else if (extention %in% c("xls", "xlsx")) {
    writexl::write_xlsx(
      data,
      save_path
    )
  } else {
    stop("Error: file_type must be either 'xlsx', 'xls', 'csv'.")
  }
}
