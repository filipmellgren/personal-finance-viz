assets_next <- function(assets, growth, savings){
  a_next <- assets*(1+growth) + savings
  return(a_next)
}

assets_T <- function(savings, s_g, a_g, end_time, a_start){
  t <- 0
  a_t <- a_start
  while (t < end_time) {
    savings <-  savings * (1+s_g)
    a_t <- assets_next(a_t, a_g, savings)
    t <- t + 1
  }
  return(a_t)
}

asset_target_diff <- function(savings, savings_g, asset_g, time_hz, asset_start){
  diff <- assets_T(savings, savings_g, asset_g, time_hz, asset_start) - wealth_target
  return(diff)
}

savings_start <- function(savings_g, asset_g, time_hz, asset_start){
  s <- uniroot(function(x){asset_target_diff(x, savings_g, asset_g, time_hz, asset_start)}, interval = c(0, 15000*12), tol = 1000)$root/12
  return(s)
}