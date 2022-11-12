#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param x an object inherited from a data.frame with columns X, Y, Z, and time
#' sorted in time.
#' @param take_log should the log of the modulus be taken. (Default is `TRUE`)
#' @param inverse should the unnormalized inverse transform is computed.
#' (Default is `TRUE`)
#' @return a data frame with the modulus of the Fourier coefficients for the
#' X, Y, and Z channels.
#' @importFrom purrr map_dfc
#' @importFrom dplyr vars mutate_at select
#' @aliases spec_sig
#' @export
spectral_signature = function(x, take_log = FALSE, inverse = TRUE) {

  ret = map_dfc(
    x |> select(X, Y, Z),
    ~ fft(.x, inverse = inverse) |> Mod()
  )
  if (take_log) {
    ret = ret |>
      mutate_at(vars(X, Y, Z), log)
  }
  ret = ret[seq_len(ceiling(nrow(ret)/2)), ]
  longest_period =
    as.numeric(difftime(max(x$time), min(x$time), units = "secs"))
  xt = x$time[1:2]
  shortest_period = as.numeric(difftime(max(xt), min(xt), units = "secs"))
  ret$freq = 1/seq(longest_period, shortest_period, length.out = nrow(ret))
  ret
}

#' @export
spec_sig = spectral_signature

