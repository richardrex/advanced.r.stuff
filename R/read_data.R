#' Data reading function
#'
#' @param dir dir() all raw files
#' @param na.rm FALSE by default
#'
#' @export readDT
#' @examples
readDT <- function(dir, na.rm = FALSE) {
  output <- list()
  for(i in c(1:length(dir))) {
    data_table <- lapply(dir, readRDS)
    names(data_table) <- dir
    data_table <- rbindlist(data_table, idcol = 'SID')
    output[[i]] <- as.data.table(data_table, na.rm = na.rm)
  }
  rbindlist(output)
}
