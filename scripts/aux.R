
# colours
colours_elev <- c("low" = "red", "low-Dec" ="orange", "medium" = "blue", "high" = "darkgreen")

colours_temp <- c("tmin" = "#a6bddb", "tmed" = "#67a9cf", "tmax" = "#016c59")


lines_lm <- c("sig" = "solid", "no sig" = "longdash")

lines_lm_size <- c("sig" = .8, "no sig" = 1)

# colours_sp <- c("pinaster" = "#f9e900", "nigra" = "darkgreen", "halepensis" = "orange", "sylvestris" = "#67a9cf")
colours_sp <- c("halepensis" = "#c1666b",
                "pinaster" = "#ecb42e",
                "nigra" = "#43ba85",
                "sylvestris" = "#006494")


colours_Specie <- c("P. halepensis" = "#c1666b",
                "P. pinaster" = "#ecb42e",
                "P. nigra" = "#43ba85",
                "P. sylvestris" = "#006494")


# pinaster, "#ffffbf", "beige" or "#f9e900"
# nigra, "#abdda4", "lightgreen"
# halepensis, "orange", "orange"
# sylvestris, "lightblue", "lightblue"

# shape
# shape_elev <- c("low" = 16, "low2" = 18, "medium" = 15, "high" = 17)
shape_elev <- c("low" = 21, "low-Dec" = 25, "medium" = 22, "high" = 24)



#### Functions to extract results from MLE
# Define custom functions to extract data from models
MLE_results_format <- function(x, yvar) {

  # compute RMSE
  x$source_data$residual <- x$source_data[[yvar]] - x$source_data[["predicted"]]
  rmse <- sqrt(sum(x$source_data$residual^2)/(length(x$source_data$residual)-1))

  out <- data.frame(
    max_likeli = x$max_likeli,
    n_params = length(x$best_pars),
    aic_cor = x$aic_corr,
    aic = x$aic,
    R2 = x$R2,
    slope = x$slope,
    RMSE = rmse
  )
  return(out)
}

MLL_results_params <- function(x) {
  params <- map_dfr(
    seq_along(x$best_pars),
    ~ {
      data.frame(
        name = names(x$best_pars)[.x],
        value = x$best_pars[.x],
        si_lower = x$lower_limits[.x],
        si_upper = x$upper_limits[.x],
        std_error = x$std_errs[.x],
        bound_lower = x$par_lo[.x],
        bound_upper = x$par_hi[.x]
      )
    }
  )

  return(params)
}


# Function to compute deltaAIC
dAIC <- function(x) {
  x <- x[!is.na(x)]
  delta.aic <- x - min(x, na.rm = TRUE)
  return(delta.aic)
}


# Combined function to plot observed vs. predicted or residuals vs. predicted

plot_mle <- function(x, yvar, title = NULL, plot_type = c("observed", "residuals")) {
  plot_type <- match.arg(plot_type)

  data <- x$source_data |>
    mutate(residuals = !!sym(yvar)  - predicted) |>
    rename(observed = !!sym(yvar))

  p <- ggplot(data, aes(x = predicted)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    ggtitle(title) +
    labs(x = "Predicted")

  if (plot_type == "observed") {
    p <- p + geom_point(aes(y = observed)) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      labs(y = "Observed")
  } else if (plot_type == "residuals") {
    p <- p + geom_point(aes(y = residuals)) +
      geom_hline(yintercept = 0, color = "red") +
      labs(y = "Residuals")
  }

  return(p)
}


# plot_mle(x = result_mtabi1, yvar = "abi",  plot_type = "observed")
# plot_mle(x = result_mtabi1, yvar = "abi",  plot_type = "residuals")


