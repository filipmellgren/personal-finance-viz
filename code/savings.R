assets_next <- function(assets, growth, savings){
  a_next <- assets*(1+growth) + savings
  return(a_next)
}

assets_seq <- function(savings, a_g, time_hz, a_start){
  assets <- numeric(time_hz)
  assets[1] <- a_start
  for (year in 2:time_hz) {
    assets[year] <- assets[year-1] * (1+a_g) + savings[year]
  }
  return(assets)
}

