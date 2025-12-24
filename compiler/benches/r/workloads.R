library(jsonlite)

bench <- function(name, iterations, fn) {
  fn() # warm-up
  times <- numeric(iterations)
  meta <- list()
  for (i in seq_len(iterations)) {
    t0 <- proc.time()[["elapsed"]]
    meta <- fn()
    t1 <- proc.time()[["elapsed"]]
    times[i] <- (t1 - t0) * 1000.0
  }
  list(
    name = name,
    iterations = iterations,
    avg_time_ms = mean(times),
    min_time_ms = min(times),
    max_time_ms = max(times),
    rss_mb = NA,
    meta = lapply(meta, as.character)
  )
}

keyword_scan <- function() {
  text <- "Patient SSN: 123-45-6789. social security number present."
  lower <- tolower(text)
  hits <- 0
  if (grepl("ssn", lower, fixed = TRUE)) hits <- hits + 1
  if (grepl("social security", lower, fixed = TRUE)) hits <- hits + 1
  list(hits = hits)
}

stats_mean <- function() {
  xs <- (0:4999 %% 100) / 10.0
  m <- mean(xs)
  list(mean = sprintf("%.6f", m))
}

dot_product <- function() {
  a <- c(0.4, 0.2, 0.9)
  w <- c(0.2, 0.5, 0.3)
  score <- sum(a * w) + 0.1
  list(score = sprintf("%.6f", score))
}

subresults <- list(
  bench("r_keyword_scan", 200, keyword_scan),
  bench("r_mean", 500, stats_mean),
  bench("r_dot_product", 2000, dot_product)
)

out <- list(
  name = "r_workloads",
  iterations = 0,
  avg_time_ms = 0.0,
  min_time_ms = 0.0,
  max_time_ms = 0.0,
  rss_mb = NA,
  meta = list(subresults = toJSON(subresults, auto_unbox = TRUE))
)

cat(toJSON(out, auto_unbox = TRUE))
