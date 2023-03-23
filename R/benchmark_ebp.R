# Internal documentation -------------------------------------------------------

# Benchmark function for the EBP

# This function is called within the EBP-function and the agruments benchmark
# and benchmark_type are documented there.

benchmark_ebp <- function (point_estim, framework, fixed, benchmark, benchmark_type) {

  if (!is.numeric(benchmark)) {

    benchmark_ <- rep(NA, length(benchmark))
    names(benchmark_) <- benchmark

    if (is.list(point_estim)) {# for point_estimation.R

      estim <- as.list(point_estim$ind[benchmark])

      for (i in benchmark) {# weighted national level
        if (i == "Mean") {
          benchmark_[i] <- weighted.mean(framework$smp_data[[paste0(fixed[2])]],
                    framework$smp_data[[framework$weights]])
        } else if (i == "Head_Count") {
          benchmark_[i] <-
            weighted.mean(framework$smp_data[[paste0(fixed[2])]] <
                            framework$threshold,
                          framework$smp_data[[framework$weights]])
        }
      }

    } else {# for mse_estimation.R

      estim <- as.list(as.data.frame(point_estim)[benchmark])
      for (i in benchmark) {# MSE - no weights in bootstrap sample
        if (i == "Mean") {
          benchmark_[i] <- mean(framework$smp_data[[paste0(fixed[2])]])
        } else if (i == "Head_Count") {
          benchmark_[i] <- mean(framework$smp_data[[paste0(fixed[2])]] <
                                           framework$threshold)
        }
      }
    }

    benchmark <- benchmark_

  } else {# raking and ratio with fixed national value
    if (is.list(point_estim)) {
      estim <- as.list(point_estim$ind[names(benchmark)])
    } else {
      estim <- as.list(as.data.frame(point_estim)[names(benchmark)])
    }
  }

  EBP_bench <- as.list(as.data.frame(
    matrix(NA, nrow = length(estim[[1]]), ncol = length(benchmark))
  ))
  names(EBP_bench) <- names(benchmark)

  share <- framework$n_pop / framework$N_pop

  for(i in names(benchmark)) {

    if (benchmark_type == "raking") {
      EBP_bench[[i]] <- estim[[i]] + benchmark[[i]] - sum(share * estim[[i]])
    } else if (benchmark_type == "ratio") {
      phi <- share / estim[[i]]
      EBP_bench[[i]] <- estim[[i]] + (1 / (sum(share^2 / phi))) *
        (benchmark[[i]] - sum(share * estim[[i]])) * (share / phi)
    }
  }

  names(EBP_bench) <- c(paste0(names(benchmark),"_bench"))

  if (is.list(point_estim)) {
    point_estim_bench <- data.frame(point_estim$ind, EBP_bench)
  } else {
    point_estim_bench <- as.matrix(data.frame(point_estim, EBP_bench))
  }


  return(point_estim_bench)
}
