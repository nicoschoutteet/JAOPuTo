#' JAOPuTo_get: helper function
#'
#' @description
#' Helper function that sends one or more GET requests to the JAO Publication Tool API,
#' automatically splitting requests into 2-day intervals, paginating over all results,
#' and applying rate limiting.
#'
#' @param dataset Character string; dataset name in the API ("core", "coreID", "ibwt", "nordic", "swe")
#' @param endpoint Character string; specific endpoint under the dataset in the API
#' @param start Start datetime; (POSIXct, Date, or character convertible to POSIXct)
#' @param end End datetime; (POSIXct, Date, or character convertible to POSIXct)
#' @param skip Integer; initial pagination offset (per 2-day chunk)
#' @param take Integer; pagination size (number of records per API call)
#' @param rate_limit_per_minute Integer; maximum number of requests per minute
#' @param ... Additional query parameters passed to the API
#'   (e.g. `presolved = TRUE`).
#'
#' @return A tibble containing the combined data returned by all API requests.
#' @examples
#' \dontrun{
#' # Zonder extra query params
#' JAOPuTo_get(
#'   dataset  = "core",
#'   endpoint = "api/data/maxNetPos",
#'   start    = "2025-01-01 00:00",
#'   end      = "2025-01-10 23:00"
#' )
#'
#' # Met extra query param 'presolved'
#' JAOPuTo_get(
#'   dataset  = "core",
#'   endpoint = "api/data/finalDomains",
#'   start    = "2025-01-01 00:00",
#'   end      = "2025-01-10 23:00",
#'   presolved = TRUE
#' )
#' }
#' @details
#' The function automatically splits requests into 2-day chunks because the API
#' does not allow long time intervals. Within each chunk, it paginates over all
#' results (in steps of `take`) until no more data are available. Rate limiting
#' is enforced to stay below the API limit of 100 requests per minute. Any
#' additional query parameters passed via `...` (e.g. `presolved`) are added to
#' each request.
JAOPuTo_get <- function(dataset,
                        endpoint,
                        start,
                        end,
                        skip = 0L,
                        take = 10000L,
                        rate_limit_per_minute = 80,
                        max_retries_429 = 5L,
                        ...) {

  base_url <- "https://publicationtool.jao.eu"

  # Extra query-parameters via ...
  extra_query <- list(...)
  reserved <- c("fromUtc", "toUtc", "skip", "take")

  # Negeer evt. conflicterende namen in ...
  if (length(extra_query)) {
    conflict <- intersect(names(extra_query), reserved)
    if (length(conflict) > 0) {
      warning(
        "JAOPuTo_get(): ignoring extra query parameters that conflict with ",
        paste(reserved, collapse = ", "), ": ",
        paste(conflict, collapse = ", ")
      )
      extra_query <- extra_query[setdiff(names(extra_query), reserved)]
    }
  }

  # Tijd tussen requests obv rate limiting (optioneel, extra voorzichtig)
  sleep_time <- if (is.finite(rate_limit_per_minute) &&
                    rate_limit_per_minute > 0) {
    60 / rate_limit_per_minute
  } else {
    0
  }

  start_utc <- as.POSIXct(start, tz = "UTC")
  end_utc   <- as.POSIXct(end,   tz = "UTC") + lubridate::hours(1)

  if (end_utc < start_utc) {
    stop("JAOPuTo_get(): 'end' is before 'start'.", call. = FALSE)
  }

  to_utc_iso <- function(x) {
    format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  # 2-daagse intervallen
  days <- seq(as.Date(start_utc), as.Date(end_utc), by = "2 day")

  dataset_clean  <- sub("/+$", "", dataset)
  endpoint_clean <- sub("^/+", "", endpoint)

  results   <- list()
  meta_list <- list()

  for (i in seq_along(days)) {

    if (i == 1) {
      day_start <- start_utc
    } else {
      day_start <- as.POSIXct(days[i], tz = "UTC")
    }

    if (i == length(days)) {
      day_end <- end_utc
    } else {
      day_end <- as.POSIXct(days[i + 1], tz = "UTC") - 1
    }

    url <- paste(base_url, dataset_clean, endpoint_clean, sep = "/")

    # Paginering binnen de 2-daagse chunk
    current_skip <- skip

    repeat {
      base_query <- list(
        fromUtc = to_utc_iso(day_start),
        toUtc   = to_utc_iso(day_end),
        skip    = current_skip,
        take    = take
      )

      # Merge basis + extra query-params
      query <- c(base_query, extra_query)

      # ---- NIEUW: 429-handling met retries ----
      attempt <- 1L
      repeat {
        resp <- httr::GET(url, query = query)
        status <- httr::status_code(resp)

        if (status == 429L) {
          # Kijk of er een Retry-After header is
          retry_after_hdr <- httr::headers(resp)[["Retry-After"]]
          retry_after <- suppressWarnings(as.numeric(retry_after_hdr))

          if (is.na(retry_after) || is.null(retry_after)) {
            # fallback: exponentiële backoff (in seconden)
            retry_after <- 30 * attempt  # 30s, 60s, 90s, ...
          }

          message(
            "JAOPuTo_get(): HTTP 429 voor chunk ", i, "/", length(days),
            " (skip = ", current_skip, "), poging ", attempt,
            ". Wachten ", retry_after, " seconden..."
          )

          if (attempt >= max_retries_429) {
            stop(
              "JAOPuTo_get(): te veel opeenvolgende 429-responses; ",
              "aborting after ", max_retries_429, " retries.",
              call. = FALSE
            )
          }

          Sys.sleep(retry_after)
          attempt <- attempt + 1L
          next   # zelfde request opnieuw proberen
        }

        if (httr::http_error(resp)) {
          stop(
            "JAOPuTo_get(): HTTP error (",
            status, ") voor chunk ",
            i, "/", length(days), " (skip = ", current_skip, ").",
            call. = FALSE
          )
        }

        # Succes: uit deze inner repeat
        break
      }
      # ---- Einde 429-handling ----

      txt    <- httr::content(resp, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)

      if ("data" %in% names(parsed)) {
        dat <- tibble::as_tibble(parsed$data)
        meta_list[[length(meta_list) + 1L]] <-
          parsed[setdiff(names(parsed), "data")]
      } else {
        dat <- tibble::as_tibble(parsed)
        meta_list[[length(meta_list) + 1L]] <- NULL
      }

      if (nrow(dat) == 0) {
        break
      }

      results[[length(results) + 1L]] <- dat

      if (nrow(dat) < take) {
        break
      }

      current_skip <- current_skip + take

      if (sleep_time > 0) {
        Sys.sleep(sleep_time)
      }
    }
  }

  if (length(results) == 0) {
    out <- tibble::tibble()
  } else {
    out <- tibble::as_tibble(do.call(rbind, results))
  }

  attr(out, "meta") <- meta_list
  out
}
