# @file executeQuery.R
#
# Copyright 2024 Darwin EU Coordination Center
#
# This file is part of CdmOnboarding
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Darwin EU Coordination Center
# @author Peter Rijnbeek
# @author Maxim Moinat

#' Execute sql file on the given connectionDetails
#' @param outputFolder                     Path to store logs and SQL files
#' @param sqlFileName                      Name of the SQL file to execute
#' @param sqlFolder                        (Optional) Path to the SQL file (default: 'checks')
#' @param successMessage                   Message to log when the query is successful
#' @param connection                       An active connection object to use
#' @param useExecuteSql                    Boolean indicating if the query should be executed using \code{dbExecute} instead of \code{dbGetQuery}
#' @param ...                              Additional parameters to pass to SqlRender::loadRenderTranslateSql
#' @returns result of the query
executeQuery <- function(
  outputFolder,
  sqlFileName,
  sqlFolder = "checks",
  successMessage = NULL,
  connection = NULL,
  useExecuteSql = FALSE,
  ...
) {
  sql <- do.call(
    SqlRender::loadRenderTranslateSql,
    c(
      sqlFilename = file.path(sqlFolder, sqlFileName),
      packageName = "CdmOnboarding",
      dbms = connection@dbms,
      warnOnMissingParameters = FALSE,
      list(...)
    )
  )

  duration <- -1
  result <- NULL

  errorReportFile <- file.path(outputFolder, sprintf("%sErr.txt", tools::file_path_sans_ext(sqlFileName)))
  tryCatch({
    start_time <- Sys.time()

    if (useExecuteSql) {
      DatabaseConnector::executeSql(
        connection = connection,
        sql = sql,
        errorReportFile = errorReportFile,
        reportOverallTime = FALSE
      )
    } else {
      result <- DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        errorReportFile = errorReportFile
      )
      names(result) <- toupper(names(result))  # DatabaseConnector v7 compatibility
    }

    # query <- SqlRender::loadSql(sql) # no need to translate again
    # if (useExecuteSql) {
    #   DBI::dbExecute(con, query)
    # } else {
    #   result <- DBI::dbGetQuery(conn, query)
    # }

    duration <- as.numeric(difftime(Sys.time(), start_time), units = "secs")
    if (is.null(successMessage)) {
      successMessage <- sprintf("'%s' executed successfully", sqlFileName)
    }
    ParallelLogger::logInfo(sprintf("> %s in %.2f secs", successMessage, duration))
  },
  error = function(e) {
    ParallelLogger::logError(e)
    ParallelLogger::logError(sprintf("> Query failed. See '%s' for more details", errorReportFile))
  })

  return(list(result = result, duration = duration))
}
