# This takes the rds file stored in given path, and writes the docx report to the same path
path <- readline("Enter the path where CdmOnboarding results are: ")

rds <- list.files(path, '.rds')
results <- readRDS(file.path(path, rds))
results <- readRDS(path)
authors <- c('-')

# Optional, add separate DED results
path_ded <- readline("Enter the path for DED file: ")
ded_results <- readRDS(path_ded)
CdmOnboarding::exportDedResults(path_ded)
results$drugExposureDiagnostics <- ded_results

if (FALSE) {
  options(error = browser)
  devtools::install(quick = TRUE, upgrade = 'never')
  devtools::reload()
}
# Optional, make compatible with current version
results <- CdmOnboarding::compat(results)
# source('R/compat.R')
# results <- compat(results)

CdmOnboarding::generateResultsDocument(
  results = results,
  outputFolder = 'output',
  authors = authors
)

# Plot regeneration -----
library(tidyverse)
source('R/Figures.R')
print(results$databaseId)

.recordsCountPlot(results$dataTablesResults$totalRecords$result, log_y_axis = F)
.recordsCountPlot(results$dataTablesResults$recordsPerPerson$result, log_y_axis = F)

.heatMapPlot(results$dataTablesResults$dayOfTheWeek$result, 'DAY_OF_THE_WEEK')
.heatMapPlot(results$dataTablesResults$dayOfTheMonth$result, 'DAY_OF_THE_MONTH')

# Select or remove a domain to recalibrate colour scale
results$dataTablesResults$dayOfTheWeek$result %>%
  dplyr::filter(DOMAIN != 'Measurement') %>%
  .heatMapPlot('DAY_OF_THE_WEEK')

results$dataTablesResults$dayOfTheMonth$result %>%
  dplyr::filter(DOMAIN %in% c('Death', 'Drug')) %>%
  .heatMapPlot('DAY_OF_THE_MONTH')


results$dataTablesResults