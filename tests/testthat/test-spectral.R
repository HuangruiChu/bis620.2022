library(purrr)
library(dplyr)

test_that(
  "The spectral_signature() is correct for default",
  {
    data(ukb_accel)

    x <- ukb_accel[1:100, ]

    result <- spec_sig(x)

    ret <- map_dfc(
      x |> select(X, Y, Z),
      ~ fft(.x, inverse = TRUE) |> Mod()
    )
    ret <- ret[seq_len(ceiling(nrow(ret) / 2)), ]
    longest_period <-
      as.numeric(difftime(max(x$time), min(x$time), units = "secs"))
    xt <- x$time[1:2]
    shortest_period <- as.numeric(difftime(max(xt), min(xt), units = "secs"))
    ret$freq <- 1 / seq(longest_period, shortest_period, length.out = nrow(ret))
    expect_equal(result, ret)
  }
)

test_that(
  "The spectral_signature() is correct for take log",
  {
    data(ukb_accel)

    x <- ukb_accel[1:100, ]

    result <- spec_sig(x, TRUE)

    ret <- map_dfc(
      x |> select(X, Y, Z),
      ~ fft(.x, inverse = TRUE) |> Mod()
    )
    ret <- ret |>
      mutate_at(vars(X, Y, Z), log)

    ret <- ret[seq_len(ceiling(nrow(ret) / 2)), ]
    longest_period <-
      as.numeric(difftime(max(x$time), min(x$time), units = "secs"))
    xt <- x$time[1:2]
    shortest_period <- as.numeric(difftime(max(xt), min(xt), units = "secs"))
    ret$freq <- 1 / seq(longest_period, shortest_period, length.out = nrow(ret))
    expect_equal(result, ret)
  }
)

test_that(
  "The spectral_signature() is correct for inverse is FALSE",
  {
    data(ukb_accel)

    x <- ukb_accel[1:100, ]

    result <- spec_sig(x)

    ret <- map_dfc(
      x |> select(X, Y, Z),
      ~ fft(.x, inverse = FALSE) |> Mod()
    )
    ret <- ret[seq_len(ceiling(nrow(ret) / 2)), ]
    longest_period <-
      as.numeric(difftime(max(x$time), min(x$time), units = "secs"))
    xt <- x$time[1:2]
    shortest_period <- as.numeric(difftime(max(xt), min(xt), units = "secs"))
    ret$freq <- 1 / seq(longest_period, shortest_period, length.out = nrow(ret))
    expect_equal(result, ret)
  }
)
