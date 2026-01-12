# Setup for using CDMConnector dataset with Achilles results pre-loaded

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env(),
  EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
)

datasetName <- "synpuf-1k"
cdmVersion <- "5.3"

tryCatch(
  if (Sys.getenv("skip_eunomia_download_test") != "TRUE") CDMConnector::downloadEunomiaData(datasetName, cdmVersion, overwrite = TRUE),
  error = function(e) NA
)

server <- CDMConnector::eunomiaDir(datasetName, cdmVersion)
connectionDetails <- DatabaseConnector::createDbiConnectionDetails("duckdb", drv = duckdb::duckdb(), server = server)

connection <- DatabaseConnector::connect(connectionDetails)

params <- list(
  connectionDetails = connectionDetails,
  connection = connection,
  cdmSchema = 'main',
  resultsSchema = 'main',
  writeSchema = 'main',  # Required write schema for DED
  databaseId = datasetName,
  cdmVersion = cdmVersion,
  dqdJsonPath = testthat::test_path('dqd_synpuf-1k.json'),
  outputFolder = testthat::test_path(),
  baseUrl = NULL  # connection to WebAPI cannot be tested
)
