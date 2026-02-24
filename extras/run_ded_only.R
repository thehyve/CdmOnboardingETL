library(DBI)
library(RPostgres)
library(CDMConnector)
library(DrugExposureDiagnostics)
library(CdmOnboarding)

# More DBI examples: https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
  host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
)

# Alternative1: Connecting using dsn, to be set up in database driver
# con <- DBI::dbConnect(odbc::odbc(), "<your_spark_dsn>")

# Alternative2: using user/password/server
# con <- DBI::dbConnect(odbc::odbc(),
#                       Driver   = "<name of the downloaded driver>",
#                       Server   = "<your_spark_server>",
#                       UID      = "<your_spark_user>",
#                       PWD      = "<your_spark_user_password>",
#                       Port     = 1433)


cdm <- cdmFromCon(
  con,
  cdmSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
  writeSchema = Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
  .softValidation = TRUE
)

ded_start_time <- Sys.time()
dedIngredients <- CdmOnboarding::getDedIngredients()
dedResults <- DrugExposureDiagnostics::executeChecks(
  cdm = cdm,
  ingredients = dedIngredients$concept_id,
  checks = c("missing", "exposureDuration", "type", "route", "dose", "quantity", "diagnosticsSummary"),
  minCellCount = 5,
  sample = NULL,
  earliestStartDate = "2005-01-01"
)
duration <- as.numeric(difftime(Sys.time(), ded_start_time), units = "secs")
CDMConnector::cdmDisconnect(cdm)

mappingLevels <- CdmOnboarding::getMappingLevel(dedResults)

dedSummary <- list(
  result = dedResults$diagnosticsSummary,
  resultMappingLevel = mappingLevels,
  duration = duration,
  packageVersion = packageVersion(pkg = "DrugExposureDiagnostics")
)

outputPath <- './'

saveRDS(dedSummary, file.path(outputPath, "dedSummary.rds"))

# Optional, export to csv
# Provide path of the onboarding results generated earlier
path_to_onboarding_results <- "output/onboarding_results_<dbname>_<yyyymmdd>.rds"
CdmOnboarding::exportDedResults(
  results = readRDS(path_to_onboarding_results),
  df_ded = dedSummary,
  outputFolder = outputPath
)
