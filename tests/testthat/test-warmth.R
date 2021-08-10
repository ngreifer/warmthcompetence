
data("vignette_data")
vignette_data <- vignette_data[1:10,]

context("warmth function")

vign_warm <- warmth(text = vignette_data$Message, ID=vignette_data$ResponseId)

test_that("longth of vectors in & out", {
  expect_equal(length(vignette_data$Message), length(vign_warm$warmth_predictions))
})

test_that("empty or na string", {

  df_warm <- warmth(text = c("","a"))
  expect_equal(length(df_warm), 2)

  df_warm <- warmth(text = c(NA_character_,"a"))
  expect_equal(length(df_warm), 2)

  vignette_data$Message[1] <- NA_character_

  df_warm <- warmth(text = vignette_data$Message)
  expect_equal(length(df_warm$warmth_predictions), length(vignette_data$Message))

})

test_that("factor handling", {
  expect_equal(warmth(text = as.factor(vignette_data$Message)),
               warmth(text = (vignette_data$Message)))
})

test_that("text of length 0", {
  empty_open <- warmth(text = "")
  expect_true(is.numeric(empty_open))
})

test_that("text of length 1", {
  empty_open <- warmth(text=vignette_data$Message)
  expect_true(is.numeric(empty_open))
})

