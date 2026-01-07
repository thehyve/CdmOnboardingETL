#' Export DED results to CSV files from CdmOnboarding results
#' @usage Rscript exportDedCsv_cmd.R <path> <mode>
#' @param path The path to the folder containing either one or two .rds files: 
#' only the CdmOnboarding results, or also the drugExposureDiagnostics export.
#' The export csv is also written to this path.
#' @param mode if 'merge' then a new CdmOnboarding .rds and .docx are created as well,
#' including the DED results.
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  path <- args[1]
  mode <- args[2]
  if (startsWith(path, '/')) {
    absPath <- path 
  } else {
    absPath <- file.path(getwd(), path)
  }
  
  doMerge <- !is.na(mode) && mode == 'merge'

  # Get all rds files
  rdsFiles <- list.files(absPath, pattern = '\\.rds$', full.names = TRUE)
  if (length(rdsFiles) == 1) {
    print("Only one .rds file found. Assuming it contains both CdmOnboarding and DED results.")
    results <- readRDS(rdsFiles[1])
    dedResults <- NULL
  } else if (length(rdsFiles) == 2) {
    print("Two .rds files found. One should be CdmOnboarding results and the other DED results.")
    df1 <- readRDS(rdsFiles[1])
    df2 <- readRDS(rdsFiles[2])
    if ("cdmSource" %in% names(df1)) {
      results <- df1
      dedResults <- df2
    } else if ("cdmSource" %in% names(df2)) {
      results <- df2
      dedResults <- df1
    } else {
      stop("No CdmOnboarding results found in the provided .rds files.")
    }
  } else {
    stop(sprintf("%d .rds files found in the specified path: %s. Expecting one or two.", length(rdsFiles), absPath))
  }

  # Print source information
  print(sprintf('Processing DED for %s - %s', results$databaseId, results$cdmSource$CDM_RELEASE_DATE))
  CdmOnboarding::exportDedResults(
    results,
    dedResults,
    outputFolder = absPath
  )

  if (doMerge) {
    #create new directory for merged results
    outputMergedPath <- file.path(absPath, 'ded_Merge')
    dir.create(outputMergedPath)
    
    # Save new rds and .docx
    results$drugExposureDiagnostics <- dedResults
    saveRDS(results, file.path(outputMergedPath, sprintf("CdmOnboarding_%s_%s.rds", results$databaseId, format(Sys.time(), "%Y%m%d"))))

    results <- CdmOnboarding::compat(results)

    CdmOnboarding::generateResultsDocument(results, outputMergedPath, authors = c())
    print(sprintf("Merged results saved to %s", outputMergedPath))
  }
}

suppressPackageStartupMessages(
  suppressMessages(
    suppressWarnings(main())
  )
)

