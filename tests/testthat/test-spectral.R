test_that(
  "return $freq",
  {
    data(ukb_accel)
    spec_sig(ukb_accel[1:100,])
  }
)

test_that(
  "return $freq",
  {
    data(ukb_accel,take_log = TRUE)
    a=spec_sig(ukb_accel[1:100,])
  }
)

test_that(
  "return $freq",
  {
    data(ukb_accel,inverse = FALSE)
    spec_sig(ukb_accel[1:100,])
  }
)
