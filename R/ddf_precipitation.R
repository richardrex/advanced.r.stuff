# DDF(Depth-Duration-Frequency curve) for precipitation data
#' @param data, A dataset with p for precipitation, date and filenames SID
#' @export ddf_precipitation
#' @examples
ddf_precipitation <- function(dt){
  dt[, p := p * 3600]
  dt <- dt[,.(D1 = p, D2 = frollsum(p,2), D3 = frollsum(p,3), D6 = frollsum(p,6), D12 = frollsum(p,12), D24 = frollsum(p,24)), by = .(year(date), SID)]
  mdt <- melt(dt, id.vars = c('year','SID'))
  dt <- mdt[,.(depth = max(value, na.rm = TRUE)), by = .(year, variable,SID)]
  dt <- dt[,.(depth = quantile(depth, p = 1 - 1/c(2,5,10,20)), freq = c(2,5,10,20)), by = .(Duration = variable, SID)]
  dt[, hour := as.numeric(gsub('D', '', Duration))]
  dt <- dt[, .(Duration, depth, freq, hour, RCP = sapply(strsplit(SID, '_'), function(x)x[5]), GCM = sapply(strsplit(SID, '_'), function(x)x[4]), RCM = sapply(strsplit(SID, '_'), function(x)x[7]), Run = sapply(strsplit(SID, '_'), function(x)x[6]))]
}

