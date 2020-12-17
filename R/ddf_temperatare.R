#' DDF for temperature data
#'
#' @param dt A dataset with certain params as TAS(temperature), DTM(date) and SID(filenames)
#'
#' @return A dataset with new columns
#' @export ddf_temp
#'
#' @examples
ddf_temperature <- function(dt){
  dt <- dt[,.(D1 = TAS, D2 = frollsum(TAS,2), D3 = frollsum(TAS,3), D6 = frollsum(TAS,6), D12 = frollsum(TAS,12), D24 = frollsum(TAS,24)), by = .(month(DTM), SID)]
  mdt <- melt(dt, id.vars = c('month', 'SID'))
  dt <- mdt[,.(depth = max(value, na.rm = TRUE)), by = .(month, variable, SID)]
  dt <- dt[,.(depth = quantile(depth, p = 1 - 1/c(2,5,10,20), na.rm = TRUE), freq = c(2,5,10,20)), by = .(Duration = variable, SID)]
  dt[, hour := as.numeric(gsub('D', '', Duration))]
  dt <- dt[, .(Duration, depth, freq, hour, RCP = sapply(strsplit(SID, '_'), function(x)x[4]), GCM = sapply(strsplit(SID, '_'), function(x)x[3]), RCM = sapply(strsplit(SID, '_'), function(x)x[6]), Run = sapply(strsplit(SID, '_'), function(x)x[5]))]
}
