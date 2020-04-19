context("TestRunBindingCorrectly")
test_that("TestRunBindingCorrectly", {
  output <- test_r_binding(4.0, 12, "hello", flag1 = TRUE)
  expect_true(output$double_out == 5.0)
  expect_true(output$int_out == 13)
  expect_true(output$string_out == "hello2")
})

context("TestRunBindingNoFlag")
test_that("TestRunBindingNoFlag", {
  output <- test_r_binding(4.0, 12, "hello")
  expect_true(output$double_out != 5.0)
  expect_true(output$int_out != 13)
  expect_true(output$string_out != "hello2")
})

context("TestRunBindingWrongString")
test_that("TestRunBindingWrongString", {
  output <- test_r_binding(4.0, 12, "goodbye", flag1 = TRUE)
  expect_true(output$string_out != "hello2")
})

context("TestRunBindingWrongInt")
test_that("TestRunBindingWrongInt", {
  output <- test_r_binding(4.0, 15, "hello", flag1 = TRUE)
  expect_true(output$int_out != 13)
})

context("TestRunBindingWrongDouble")
test_that("TestRunBindingWrongDouble", {
  output <- test_r_binding(2.0, 12, "hello", flag1 = TRUE)
  expect_true(output$double_out != 5.0)
})

context("TestRunBadFlag")
test_that("TestRunBadFlag", {
  output <- test_r_binding(4.0, 12, "hello", flag1 = TRUE, flag2 = TRUE)
  expect_true(output$double_out != 5.0)
  expect_true(output$int_out != 13)
  expect_true(output$string_out != "hello2")
})

context("TestMatrix")
test_that("TestMatrix", {
  x <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), nrow = 5)
  y <- matrix(c(1, 2, 6, 4, 6, 7, 16, 9, 11, 12, 26, 14), nrow = 4)
  output <- test_r_binding(4.0, 12, "hello", x)
  expect_identical(output$matrix_out, y)
})

context("TestModel")
test_that("TestModel", {
  output1 <- test_r_binding(4.0, 12, "hello", build_model = TRUE)
  output2 <- test_r_binding(4.0, 12, "hello", model_in = output1$model_out)
  expect_true(output2$model_bw_out == 20)
})

context("TestSerialization")
test_that("TestSerialization", {
  output1 <- test_r_binding(4.0, 12, "hello", build_model = TRUE)
  serialize_gaussian_kernel("model.bin", output1$model_out)
  new_model <- unserialize_gaussian_kernel("model.bin")
  unlink("model.bin")
  output2 <- test_r_binding(4.0, 12, "hello", model_in = new_model)
  expect_true(output2$model_bw_out == 20)
})
