test_that("Unit tests for shiny", {
  if (!shinytest::dependenciesInstalled()) {
    shinytest::installDependencies()
  }
  
  shiny_obj <- drawr_app()

  app <- shinytest::ShinyDriver$new(shiny_obj)
  shinyDrawr <- app$findWidget("shinydrawr")
  expect_true(shinyDrawr$isOutput()[1])
  expect_equal(shinyDrawr$getHtml()[1], "<div class=\"r2d3 html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item shiny-bound-output\" id=\"shinydrawr\" style=\"width: 100%; height: 500px; visibility: inherit;\" aria-live=\"polite\"></div>")
  expect_equal(app$getTitle()[1], "Can 'You Draw It'?")
})