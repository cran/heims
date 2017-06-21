context("Relevel heims")

test_that("Works on small data.table as expected", {
  DT <- data.table(Country_of_birth = c("Australia", "Vietnam", "Australia"),
                   State_permanent_home = c("NSW", "Vic", "ACT"),
                   TER = c(90, 50, 80))

  DT_releveled <- relevel_heims(DT)


  expect_equal(levels(DT_releveled$Country_of_birth)[1],
               first_levels[Variable == "Country_of_birth"][["First_level"]])
  expect_equal(levels(DT_releveled$State_permanent_home)[1],
               first_levels[Variable == "State_permanent_home"][["First_level"]])

})
