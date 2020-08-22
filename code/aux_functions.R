# Wage ####
growth.seq <- function(g = 0, time_hz = 25, start_value){
  
  years <- 1:time_hz
  sequence <- rep(start_value*12, length(years))*exp(g*years)
  
  return(sequence)
}


# Savings ####
assets_next <- function(assets, growth, savings){
  a_next <- assets*(1+growth) + savings
  return(a_next)
}

assets_seq <- function(savings, a_g, time_hz, a_start){
  assets <- numeric(time_hz)
  assets[1] <- a_start + savings[1]
  for (year in 2:time_hz) {
    assets[year] <- assets[year-1] * exp(a_g[year]) + savings[year]
  }

  return(assets)
}
