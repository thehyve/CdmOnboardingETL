test_that("Drug Exposure Diagnostics Checks", {

  cdm <- CDMConnector::cdmFromCon(
    con = params$connection,nection,
    cdmSchema = params$cdmSchema,
    writeSchema = params$writeSchema,
    .softValidation = TRUE
  )

  dedResults <- CdmOnboarding:::.runDedChecks(
    cdm
  )

  testthat::expect_type(dedResults, 'list')
  testthat::expect_true(
    !is.null(dedResults$result),
    info = paste("The result in drugExposureDiagnostics is null")
  )
})
