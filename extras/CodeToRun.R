#' CodeToRun.R
#' 
#' This script provides an example of how to run the CdmOnboarding package.
#' It retrieves connection details from environment variables defeined your .Renviron file:
#' Note that these follow DBI driver settings and might differ per DBMS. For postgres:
#'    DBMS = "postgresql"
#'    DB_HOST = "localhost_or_other_host"
#'    DB_PORT = 5432
#'    DB_NAME = "your_database_name"
#'    DB_USER = "your_user"
#'    DB_PASSWORD = "your_secret_password"
#'    CDM_SCHEMA = "your_cdm_schema"
#'    RESULTS_SCHEMA = "your_achilles_results_schema"
#'
#' Examples for other DBMS: https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html

# Install latest version of CdmOnboarding if not already installed
if (!require(CdmOnboarding)) {
  remotes::install_github("DARWIN-EU/CdmOnboarding")
}

library(CdmOnboarding)
library(Achilles)
library(DashboardExport)
remotes::install_github('darwin-eu/DashboardExport')

achilles(
  connectionDetails,
  cdmDatabaseSchema,
  resultsDatabaseSchema = cdmDatabaseSchema,
  scratchDatabaseSchema = resultsDatabaseSchema,
  vocabDatabaseSchema = cdmDatabaseSchema,
  tempEmulationSchema = resultsDatabaseSchema,
  createTable = TRUE,
  smallCellCount = 5,
  cdmVersion = "5.4",
  createIndices = TRUE,
  numThreads = 1,
  tempAchillesPrefix = "tmpach",
  dropScratchTables = TRUE,
  sqlOnly = FALSE,
  outputFolder = "output",
  verboseMode = TRUE,
  optimizeAtlasCache = FALSE,
  defaultAnalysesOnly = FALSE,
  updateGivenAnalysesOnly = FALSE,
  sqlDialect = NULL
)

# Fill out the DBI connection details -----------------------------------------------------------------------
connectionDetails <- DatabaseConnector::createDbiConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  drv = RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Details for connecting to the CDM
cdmSchema <- Sys.getenv("CDM_SCHEMA")
resultsSchema <- Sys.getenv("RESULTS_SCHEMA")

# Details specific to the database:
databaseId <- 'Test' # Sys.getenv("DATABASE_ID")
authors <- c('C-path') # used on the title page

# (optional) URL to the WebAPI that your local Atlas instance uses, e.g. http://localhost:8080/WebAPI
baseUrl <- Sys.getenv("WEBAPI_BASEURL")

# (optional) Path to your DQD results file
dqdJsonPath <- '~/Documents/CdmOnboarding/output/results_TSC.json'

outputFolder <- file.path(getwd(), "output", databaseId)
smallCellCount <- 5
verboseMode <- TRUE

# *******************************************************
# SECTION 3: Run the package
# *******************************************************
results <- CdmOnboarding::cdmOnboarding(
  connectionDetails = connectionDetails,
  cdmSchema = cdmSchema,
  resultsSchema = resultsSchema,
  databaseId = databaseId,
  authors = authors,
  smallCellCount = smallCellCount,
  outputFolder = outputFolder,
)

# cdmOnboarding() should already generate the resultsdocument.
# Use this to regenerate upon error (results object should be returned anyway)
if (FALSE) {
  CdmOnboarding::generateResultsDocument(
    results = results,
    outputFolder = outputFolder,
    authors = authors
  )
}
