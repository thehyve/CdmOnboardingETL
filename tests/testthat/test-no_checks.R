test_that("No Checks", {
  results <- CdmOnboarding::cdmOnboarding(
    connectionDetails = params$connectionDetails,
    cdmSchema = params$cdmSchema,
    resultsSchema = params$resultsSchema,
    outputFolder = params$outputFolder,
    databaseId = params$databaseId,
    dqdJsonPath = NULL,
    baseUrl = NULL,
    runDataTablesChecks = FALSE,
    runVocabularyChecks = FALSE,
    runPerformanceChecks = FALSE,
    runWebAPIChecks = FALSE,
    runDedChecks = FALSE,
    runCohortBenchmarkChecks = FALSE
  )

  testthat::expect_type(results, 'list')
})
