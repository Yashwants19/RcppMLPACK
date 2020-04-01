context("TestRunBindingCorrectly")
test_that("TestRunBindingCorrectly", {
  output <- testRBinding(4.0, 12, "hello", flag1=TRUE)
  expect_true(output$doubleOut == 5.0)
  expect_true(output$intOut == 13)
  expect_true(output$stringOut == "hello2")
})


context("TestRunBindingNoFlag")
test_that("TestRunBindingNoFlag", {
  output <- testRBinding(4.0, 12, "hello")
  expect_true(output$doubleOut != 5.0)
  expect_true(output$intOut != 13)
  expect_true(output$stringOut != "hello2")
})

context("TestRunBindingWrongString")
test_that("TestRunBindingWrongString", {
  output <- testRBinding(4.0, 12, "goodbye", flag1=TRUE)
  expect_true(output$stringOut != "hello2")
})

context("TestRunBindingWrongInt")
test_that("TestRunBindingWrongInt", {
  output <- testRBinding(4.0, 15, "hello", flag1=TRUE)
  expect_true(output$intOut != 13)
})

context("TestRunBindingWrongDouble")
test_that("TestRunBindingWrongDouble", {
  output <- testRBinding(2.0, 12, "hello", flag1=TRUE)
  expect_true(output$doubleOut != 5.0)
})

context("TestRunBadFlag")
test_that("TestRunBadFlag", {
  output <- testRBinding(4.0, 12, "hello", flag1=TRUE, flag2=TRUE)
  expect_true(output$doubleOut != 5.0)
  expect_true(output$intOut != 13)
  expect_true(output$stringOut != "hello2")
})

context("TestMatrix")
test_that("TestMatrix", {
  x <- matrix ( c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), nrow = 5)
  y <- matrix ( c( 1, 2, 6, 4, 6, 7, 16, 9, 11, 12, 26, 14), nrow = 4)
  output <- testRBinding(4.0, 12, "hello", x)
  expect_identical(output$matrixOut, y)
})

context("TestModel")
test_that("TestModel", {
  output1 <- testRBinding(4.0, 12, "hello", buildModel=TRUE)
  output2 <- testRBinding(4.0, 12, "hello", modelIn=output1$modelOut)
  expect_true(output2$modelBwout == 20)
})