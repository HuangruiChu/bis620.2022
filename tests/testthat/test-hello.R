test_that(
  "The hello is visible and returns 'Hello'.",
  {
    expect_equal(hello(), "Hello!")
  }
)

test_that(
  "The hello is visible and returns 'Hello + name'.",
  {
    expect_equal(hello("CHU"), "Hello CHU!")
  }
)

test_that(
  "The data is invisible",
  {
    expect_equal(hello("CHU", TRUE), invisible("Hello CHU!"))
  }
)
