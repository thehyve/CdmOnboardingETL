test_that("Benchmark Checks", {
  cdm <- CDMConnector::cdmFromCon(
    con = params$connection,
    cdmSchema = params$cdmSchema,
    writeSchema = params$writeSchema,
    .softValidation = TRUE
  )

  cohortBenchmarkResults <- CdmOnboarding:::.runCohortBenchmark(
    cdm
  )

  testthat::expect_type(cohortBenchmarkResults, 'list')
})