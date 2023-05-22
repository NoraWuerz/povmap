compute_gini <- function(welfare) {


  welfare <- welfare[1:length(welfare)/2]
  weight <- welfare[(length(welfare)/2) + 1 : length(welfare)]

  if (is.null(weight)){
    weight <- rep(1, length(welfare))
  }

  df <- data.frame(welfare = welfare,
                   weight = weight)

  df <- df[complete.cases(df),]

  welfare <- df$welfare
  weight <- df$weight

  weight <- weight/sum(weight)

  order <- order(welfare)
  welfare <- welfare[order]
  weight <- weight[order]
  p <- cumsum(weight)
  nu <- cumsum(weight * welfare)
  n <- length(nu)
  nu <- nu/nu[n]
  gini <- sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])

  return(gini)
}

compute_gap <- function(welfare, threshold){

  welfare <- welfare[1:length(welfare)/2]
  weight <- welfare[(length(welfare)/2) + 1 : length(welfare)]

  df <- data.frame(welfare = welfare,
                   weight = weight)

  df <- df[complete.cases(df),]

  welfare <- df$welfare
  weight <- df$weight


  pov_status <- (welfare < threshold)

  relative_distance <- (1 - (welfare[pov_status] / threshold))

  weight_pov <- weight[pov_status]

  weight_total <- sum(weight)

  fgt1 <- sum(relative_distance * weight_pov) / weight_total

  return(fgt1)

}

compute_headcount <- function(welfare, threshold){

  welfare <- welfare[1:length(welfare)/2]
  weight <- welfare[(length(welfare)/2) + 1 : length(welfare)]

  df <- data.frame(welfare = welfare,
                   weight = weight)

  df <- df[complete.cases(df),]

  welfare <- df$welfare
  weight <- df$weight

  pov_status <- as.integer(welfare < threshold)

  fgt0 <- sum(pov_status * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)

  return(fgt0)

}

present_modelselect <- function(dt,
                                y,
                                xvars,
                                weights,
                                cluster_id){

  dt <- as.data.table(dt)

  if(is.null(weights) == FALSE){

    dt <- na.omit(dt[,c(y, xvars, weights, cluster_id), with = F])

    weights <- dt[, weights, with = F]

    #weights <- scale(weights)

  } else {

    dt <- na.omit(dt[,c(y, xvars, cluster_id), with = F])

    weights <- 1

  }

  xset <- dt[, xvars, with = F]

  y <- dt[, y, with = F]

}

weighted.sd <- function(x, w){

  delta_sq <- (x - mean(x))^2
  nzero_w <- (length(w[w > 0]) - 1) / length(w[w > 0])
  result <- sqrt(sum(w * (delta_sq)) / (nzero_w * sum(w)))

  return(result)
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

wgt.mean <- function(x, na.rm) {

  x <- x[1:length(x)/2]
  w <- x[(length(x)/2) + 1 : length(x)]

  df <- data.frame(x = x,
                   w = w)

  df <- df[complete.cases(df),]

  w <- df$w
  x <- df$x

  y <- sum(w * x, na.rm = na.rm) / sum(w, na.rm = na.rm)

  return(y)

}

create_calibmatrix <- function(x){


  unique_obs <- unique(x)

  result <-
    lapply(unique_obs,
           function(y) {

             z <- as.integer(y == x)

             return(z)

           })

  result <- do.call(cbind, result)

  colnames(result) <- unique_obs

  return(result)

}
