test_that("Unit tests for shiny", {
  if (!shinytest::dependenciesInstalled()) {
    shinytest::installDependencies()
  }
  
  shiny_obj <- drawr_app()

  app <- shinytest::ShinyDriver$new(shiny_obj)
  shinyDrawr <- app$findWidget("shinydrawr")
  expect_true(shinyDrawr$isOutput()[1])
  expect_equal(app$getTitle()[1], "Can 'You Draw It'?")
})