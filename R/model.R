#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' @importFrom stats predict
train_timegpt <- function(.data, specials, ...){
  # if(length(tsibble::measured_vars(.data)) > 1){
  #   abort("Only univariate responses are supported by TimeGPT")
  # }

  # Return model
  structure(
    list(
      .data = .data,
      par = list(...)
    ),
    class = "fbl_timegpt"
  )
}

specials_timegpt <- new_specials(
  xreg = function(..., prior_scale = NULL, standardize = "auto", type = NULL){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = reduce(c(0, enexprs(...)), function(.x, .y) call2("+", .x, .y))
    )
    list(
      xreg = model.matrix(model_formula, self$data),
      prior_scale = prior_scale,
      standardize = standardize,
      mode = type
    )
  }
)

#' TimeGPT SDK for generative forecasting
#'
#' Provides a fable compatible model to obtain forecasts using the Nixtla's
#' TimeGPT API.
#'
#' The TimePGT modelling interface uses a `formula` based model specification
#' (`y ~ x`), where the left of the formula specifies the response variable,
#' and the right specifies the model's predictive terms. Like any model in the
#' fable framework, it is possible to specify transformations on the response.
#'
#' The introduction vignette provides more details on how to model data
#' using this interface to the TimeGPT API: `vignette("intro", package="fable.timegpt")`.
#'
#' @param formula A symbolic description of the model to be fitted of class `formula`.
#' @param ... Additional arguments passed to the API (see <https://docs.nixtla.io/reference/timegpt_timegpt_post>)
#'
#' @section Specials:
#'
#' There are no specials for this forecasting model.
#'
#' @seealso
#' - [TimeGPT homepage](https://docs.nixtla.io/)
#' - [TimeGPT documentation](https://docs.nixtla.io/docs)
#' - [TimeGPT Python SDK](https://docs.nixtla.io/docs/python-sdk-installation)
#'
#' @examples
#' library(tsibble)
#' as_tsibble(USAccDeaths) %>%
#'   model(
#'     timegpt = TimeGPT(value)
#'   )
#'
#' @export
TimeGPT <- function(formula, ...){
  timegpt <- new_model_class("timegpt", train_timegpt, specials_timegpt)
  new_model_definition(timegpt, !!rlang::enquo(formula), ...)
}

#' Produce forecasts from the TimeGPT API
#'
#' @inheritParams fable::forecast.ARIMA
#' @param ... Unused.
#' @param token The API token obtained from <https://dashboard.nixtla.io/>
#' @param level The level(s) of uncertainty to produce conformal forecast intervals for.
#'
#' @return A fable.
#'
#' @export
forecast.fbl_timegpt <- function(object, new_data, specials = NULL, times = 1000, level = NULL, token = Sys.getenv("TIMEGPT_TOKEN"), ...){
  tsbl <- object$.data

  # Call the API
  y <- tsbl[[tsibble::measured_vars(tsbl)]]
  names(y) <- as.Date(tsbl[[tsibble::index_var(tsbl)]])
  # y <- jsonlite::toJSON(as.list(y), auto_unbox = TRUE)

  valid_intvls <- list(
    "H" = tsibble::new_interval(hour = 1),
    "D" = tsibble::new_interval(day = 1),
    "W" = tsibble::new_interval(week = 1),
    "M" = tsibble::new_interval(month = 1)
  )
  data_intvl <- vapply(valid_intvls, identical, logical(1L), tsibble::interval(tsbl))
  if(!any(data_intvl)) {
    stop("The data must be of a hourly, daily, weekly, or monthly interval.")
  }

  resp <- httr2::request("https://dashboard.nixtla.io/api/timegpt") |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
    ) |>
    httr2::req_body_json(
      c(
        list(
          fh = nrow(new_data),
          y = as.list(y),
          freq = names(which(data_intvl)),
          level = level
        ),
        list(...)
      )
    ) |>
    httr2::req_perform()

  fc <- httr2::resp_body_json(resp)

  names(fc$data)

  # Return forecasts
  if(!is.null(level)) {
    upper <- 0.5 * (1 + level/100)
    lower <- 0.5 * (1 - level/100)
    is_lo <- grepl("^lo-", names(fc$data))
    is_hi <- grepl("^hi-", names(fc$data))
    is_int <- is_lo|is_hi
    is_val <- names(fc$data) == "value"

    conf <- as.numeric(sub("^(lo|hi)-(.*)", "\\2", names(fc$data)[is_int]))

    dist_symmetric_percentile(
      x = unname(split(unname(unlist(fc$data[is_val|is_int])), rep(seq_len(nrow(new_data)), 1 + length(level) * 2))),
      percentile = rep(list(c(0.5, 0.5 * (1 + (1 - is_lo[is_int]*2)*conf/100))), nrow(new_data))
    )
  } else {
    distributional::dist_degenerate(unlist(fc$data$value))
  }
}

#' Obtain historical 1-step forecasts from TimeGPT Historic
#'
#' Based on the provided data, this endpoint predicts time series data for the
#' in-sample period (historical period). It takes a JSON as an input, including
#' information like the series’ frequency and the historical data.
#'
#' @inheritParams fable::fitted.ARIMA
#' @inheritParams forecast.fbl_timegpt
#'
#' @return A vector of fitted values.
#'
#' @export
fitted.fbl_timegpt <- function(object, token = Sys.getenv("TIMEGPT_TOKEN"), ...){
  tsbl <- object$.data

  # Call the API
  y <- tsbl[[tsibble::measured_vars(tsbl)]]
  names(y) <- as.Date(tsbl[[tsibble::index_var(tsbl)]])
  # y <- jsonlite::toJSON(as.list(y), auto_unbox = TRUE)

  valid_intvls <- list(
    "H" = tsibble::new_interval(hour = 1),
    "D" = tsibble::new_interval(day = 1),
    "W" = tsibble::new_interval(week = 1),
    "M" = tsibble::new_interval(month = 1)
  )
  data_intvl <- vapply(valid_intvls, identical, logical(1L), tsibble::interval(tsbl))
  if(!any(data_intvl)) {
    stop("The data must be of a hourly, daily, weekly, or monthly interval.")
  }

  resp <- httr2::request("https://dashboard.nixtla.io/api/timegpt_historic") |>
    httr2::req_headers(
      "accept" = "application/json",
      "content-type" = "application/json",
      "authorization" = paste("Bearer", token)
    ) |>
    httr2::req_body_json(
      c(
        list(
          y = as.list(y),
          freq = names(which(data_intvl))#,
          # level = level
        ),
        list(...)
      )
    ) |>
    httr2::req_perform()

  content <- httr2::resp_body_json(resp)
  fit <- unlist(content$data$value)
  c(rep(NA_real_, nrow(tsbl) - length(fit)), fit)
}

dist_symmetric_percentile <- function(x, percentile) {
  distributional::new_dist(x = x, percentile = percentile, class = c("dist_symmetric_percentile", "dist_percentile"))
}

#' @export
mean.dist_symmetric_percentile <- function(x, ...){
  median(x, ...)
}

#' @export
model_sum.fbl_timegpt <- function(x){
  "timegpt"
}

#' @export
format.fbl_timegpt <- function(x, ...){
  "TimeGPT Model"
}
