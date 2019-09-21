id = "maca"
startDate = "1950-01-01"
endDate = "2099-12-31"
scenario = "rcp45"
param = "tmax"
model = "CCSM4"
base = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_"
if (!timeRes %in% c("daily", "monthly")) {
  stop("timeRes must be monthly or daily")
}
d = climateR:::define.dates(startDate, endDate, baseDate = "1950-01-01",
                 splitDate = "2006-01-01")
v = climateR:::define.versions(dates = d,
                               scenario = scenario,
                               future.call = paste0("2006_2099_CONUS_", timeRes, ".nc?"),
                               historic.call = paste0("1950_2005_CONUS_", timeRes, ".nc?"),
                               timeRes = timeRes)
p = climateR:::define.param(param, service = "maca")
k = climateR:::define.config(dataset = "maca", model = model, ensemble = NA)
tmp = expand.grid(min.date = v$min.date, model = k, call = p$call,
                  stringsAsFactors = FALSE)
fin = merge(v, tmp, "min.date") %>% merge(p, "call") %>%
  merge(model_meta$maca, "model")

date.names = d$date
g = climateR:::define.grid3(AOI, source = id)
urls = paste0(base, fin$call, "_", fin$model, "_", fin$ensemble,
              "_", fin$ver, "_", fin$calls, fin$call2, fin$time.index,
              g$lat.call, g$lon.call)
pp = paste0(fin$model, "_", fin$common.name)
s = climateR:::fast.download(urls, params = fin$call2, names = pp,
                             g, name.date, dataset = id, fun = "r")

