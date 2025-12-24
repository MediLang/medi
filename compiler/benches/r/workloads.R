library(jsonlite)

parse_args <- function(argv) {
  cfg <- list(
    tier = "",
    obs_n = 20000,
    threads = 4,
    iterations_fast = 1000,
    iterations_medium = 20,
    iterations_slow = 10,
    repeats = 1
  )
  i <- 1
  while (i <= length(argv)) {
    a <- argv[[i]]
    if (a == "--tier" && i + 1 <= length(argv)) {
      cfg$tier <- argv[[i + 1]]
      i <- i + 2
    } else if (a == "--obs-n" && i + 1 <= length(argv)) {
      cfg$obs_n <- as.integer(argv[[i + 1]])
      i <- i + 2
    } else if (a == "--threads" && i + 1 <= length(argv)) {
      cfg$threads <- as.integer(argv[[i + 1]])
      i <- i + 2
    } else if (a == "--iterations-fast" && i + 1 <= length(argv)) {
      cfg$iterations_fast <- as.integer(argv[[i + 1]])
      i <- i + 2
    } else if (a == "--iterations-medium" && i + 1 <= length(argv)) {
      cfg$iterations_medium <- as.integer(argv[[i + 1]])
      i <- i + 2
    } else if (a == "--iterations-slow" && i + 1 <= length(argv)) {
      cfg$iterations_slow <- as.integer(argv[[i + 1]])
      i <- i + 2
    } else if (a == "--repeats" && i + 1 <= length(argv)) {
      cfg$repeats <- as.integer(argv[[i + 1]])
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
  cfg$threads <- max(1L, cfg$threads)
  cfg$repeats <- max(1L, cfg$repeats)
  cfg
}

mean_safe <- function(xs) {
  if (length(xs) == 0) {
    return(0)
  }
  mean(xs)
}

stddev_sample <- function(xs) {
  n <- length(xs)
  if (n < 2) {
    return(0)
  }
  mu <- mean(xs)
  sqrt(sum((xs - mu) ^ 2) / (n - 1))
}

synthetic_lab_results <- function(n) {
  ids <- sprintf("obs-%d", 0:(n - 1))
  code <- ifelse((0:(n - 1)) %% 10 == 0, "HGB", "WBC")
  value <- ((0:(n - 1)) %% 100) / 10.0
  unit <- rep("g/dL", n)
  data.frame(id = ids, code = code, value = value, unit = unit, stringsAsFactors = FALSE)
}

validate_observation <- function(df) {
  all(!is.na(df$id)) && all(nchar(df$id) > 0) &&
    all(!is.na(df$code)) && all(nchar(df$code) > 0) &&
    all(!is.na(df$value)) && all(is.finite(df$value)) &&
    all(!is.na(df$unit)) && all(nchar(df$unit) > 0)
}

add_common_meta <- function(meta, cfg) {
  m <- meta
  if (!is.null(cfg$tier) && nchar(cfg$tier) > 0) {
    m$tier <- cfg$tier
  }
  m$obs_n <- as.character(cfg$obs_n)
  m$threads <- as.character(cfg$threads)
  m
}

bench <- function(name, iterations, repeats, fn) {
  times <- c()
  meta <- list()
  repeats <- max(1L, repeats)
  for (r in seq_len(repeats)) {
    fn() # warm-up once per repeat
    for (i in seq_len(iterations)) {
      t0 <- proc.time()[["elapsed"]]
      meta <- fn()
      t1 <- proc.time()[["elapsed"]]
      times <- c(times, (t1 - t0) * 1000.0)
    }
  }
  avg <- mean_safe(times)
  sd <- stddev_sample(times)
  cv <- ifelse(abs(avg) > 1e-12, (sd / avg) * 100.0, 0.0)
  list(
    name = name,
    iterations = iterations,
    repeats = repeats,
    avg_time_ms = avg,
    min_time_ms = min(times),
    max_time_ms = max(times),
    stddev_time_ms = sd,
    cv_pct = cv,
    rss_mb = NA_real_,
    meta = lapply(meta, as.character)
  )
}

cfg <- parse_args(commandArgs(trailingOnly = TRUE))

keyword_scan <- function() {
  text <- "Patient SSN: 123-45-6789. social security number present."
  lower <- tolower(text)
  hits <- 0
  if (grepl("ssn", lower, fixed = TRUE)) hits <- hits + 1
  if (grepl("social security", lower, fixed = TRUE)) hits <- hits + 1
  add_common_meta(list(hits = hits), cfg)
}

stats_mean <- function() {
  xs <- (0:4999 %% 100) / 10.0
  m <- mean(xs)
  add_common_meta(list(mean = sprintf("%.6f", m)), cfg)
}

dot_product <- function() {
  a <- c(0.4, 0.2, 0.9)
  w <- c(0.2, 0.5, 0.3)
  score <- sum(a * w) + 0.1
  add_common_meta(list(score = sprintf("%.6f", score)), cfg)
}

observation_validate_query <- function() {
  obs <- synthetic_lab_results(cfg$obs_n)
  if (!validate_observation(obs)) {
    stop("invalid")
  }
  matched <- obs[obs$code == "HGB", ]
  add_common_meta(list(observations = nrow(obs), matched = nrow(matched)), cfg)
}

observation_ndjson_roundtrip <- function() {
  obs <- synthetic_lab_results(cfg$obs_n)
  lines <- apply(obs, 1, function(row) {
    toJSON(as.list(row), auto_unbox = TRUE)
  })
  nd <- paste(lines, collapse = "\n")
  parsed <- lapply(strsplit(nd, "\n")[[1]], function(line) {
    fromJSON(line)
  })
  add_common_meta(list(bytes = nchar(nd), items = length(parsed)), cfg)
}

observation_validate_query_parallel <- function() {
  obs <- synthetic_lab_results(cfg$obs_n)
  workers <- max(1L, min(cfg$threads, nrow(obs)))
  chunk <- as.integer(ceiling(nrow(obs) / workers))

  if (workers > 1L && requireNamespace("parallel", quietly = TRUE)) {
    starts <- seq(1L, nrow(obs), by = chunk)
    parallel::mclapply(starts, function(start) {
      end <- min(start + chunk - 1L, nrow(obs))
      slice <- obs[start:end, ]
      if (!validate_observation(slice)) {
        stop("invalid")
      }
      TRUE
    }, mc.cores = workers)
  } else {
    if (!validate_observation(obs)) {
      stop("invalid")
    }
  }

  matched <- obs[obs$code == "HGB", ]
  add_common_meta(
    list(observations = nrow(obs), matched = nrow(matched), workers = workers),
    cfg
  )
}

subresults <- list(
  bench("r_keyword_scan", cfg$iterations_fast, cfg$repeats, keyword_scan),
  bench("r_mean", cfg$iterations_fast, cfg$repeats, stats_mean),
  bench("r_dot_product", cfg$iterations_fast, cfg$repeats, dot_product),
  bench("r_observation_validate_query", cfg$iterations_medium, cfg$repeats, observation_validate_query),
  bench("r_observation_ndjson_roundtrip", cfg$iterations_slow, cfg$repeats, observation_ndjson_roundtrip),
  bench(
    "r_observation_validate_query_parallel",
    cfg$iterations_slow,
    cfg$repeats,
    observation_validate_query_parallel
  )
)

out <- list(
  name = "r_workloads",
  iterations = 0,
  repeats = 0,
  avg_time_ms = 0.0,
  min_time_ms = 0.0,
  max_time_ms = 0.0,
  stddev_time_ms = 0.0,
  cv_pct = 0.0,
  rss_mb = NA_real_,
  meta = list(
    subresults = as.character(toJSON(subresults, auto_unbox = TRUE, na = "null"))
  )
)

cat(toJSON(out, auto_unbox = TRUE, na = "null"))
