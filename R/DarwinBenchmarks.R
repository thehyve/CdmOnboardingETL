# @file Benchmarks
#
# Copyright 2026 Darwin EU Coordination Center
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


#' Generate predefined set of cohorts
#' @param cdm An R object of type \code{cdm_reference}
#' @returns dataframe with the summarised results of the cohort benchmark
.runCohortBenchmark <- function(cdm) {
  ParallelLogger::logInfo("Starting execution of Cohort Benchmark")

  start_time <- Sys.time()
  cohort_set_definition <- CDMConnector::readCohortSet(
    path = system.file("json", "cohorts", package = "CdmOnboarding", mustWork = TRUE)
  )
  n_cohorts <- nrow(cohort_set_definition)

  # Generate each cohort definition one by one to capture time taken and possible errors
  n_records <- c()
  n_subjects <- c()
  duration <- c()
  error <- c()
  for (i in seq(1, n_cohorts)) {
    start_time <- Sys.time()
    result <- tryCatch({
      suppressWarnings(suppressMessages(
        cdm <- CDMConnector::generateCohortSet(
          cdm = cdm,
          cohortSet = cohort_set_definition[i, ],
          name = "cohort",
          computeAttrition = FALSE,
          overwrite = TRUE
        )
      ))
      cohort_count <- CDMConnector::cohortCount(cdm$cohort)
      ParallelLogger::logInfo("Generated: ", cohort_set_definition[[i, 'cohort_name']])
      cohort_count
    }, error = function(e) {
      ParallelLogger::logInfo("Error in generating: ", cohort_set_definition[[i, 'cohort_name']])
      e
    })
    delta <- as.numeric(difftime(Sys.time(), start_time), units = "secs")

    if (inherits(result, "error")) {
      n_records <- c(n_records, NA)
      n_subjects <- c(n_subjects, NA)
      duration <- c(duration, delta)
      error <- c(error, result$message)
      next
    } else {
      n_records <- c(n_records, result$number_records)
      n_subjects <- c(n_subjects, result$number_subjects)
      duration <- c(duration, delta)
      error <- c(error, NA)
    }
  }

  n_subject_bins <- cut(as.integer(n_subjects), breaks = c(0, 100, 1000, 10000, 100000, 1000000, Inf), labels = c("0-100", "100-1k", "1k-10k", "10k-100k", "100k-1M", "1M+"))

  data.frame(
    cohort_name = cohort_set_definition$cohort_name,
    n_subject_bins = as.character(n_subject_bins),
    duration = duration,
    error = error
  )
}


#' Run Benchmark CDMConnector
#' @param cdm An R object of type \code{cdm_reference}
#' @returns list of DED diagnostics_summary and duration
.runBenchmarkCdmConnector <- function(cdm) {
  ParallelLogger::logInfo("Starting execution of CDMConnector Benchmark")

  start_time <- Sys.time()
  benchmarkResults <- CDMConnector::benchmarkCDMConnector(cdm)
  duration <- as.numeric(difftime(Sys.time(), start_time), units = "secs")

  # Return result with duration
  list(result = benchmarkResults, duration = duration)
}


#' Run DARWIN Analytics Benchmarks
#' @param cdm An R object of type \code{cdm_reference}
#' @returns dataframe with the summarised results of the cohort benchmark
.analyticsBenchmarks <- function(cdm) {
  ParallelLogger::logInfo("Starting execution of DARWIN Benchmarks")
  start_time <- Sys.time()

  ParallelLogger::logInfo("> (1/4) DrugUtilisation Benchmark")
  duBenchmarkResult <- tryCatch({
    cbind(
      package_name = 'DrugUtilisation',
      DrugUtilisation::benchmarkDrugUtilisation(
        cdm,
        ingredient = "acetaminophen",
        alternativeIngredient = c("ibuprofen", "aspirin", "diclofenac"),
        indicationCohort = NULL
      )
    )
  }, error = function(e) {
    ParallelLogger::logError("Execution of DrugUtilisation Benchmark failed: ", e)
    NULL
  })
    
  ParallelLogger::logInfo("> (2/4) CodeListGenerator Benchmark")
  cgBenchmarkResult <- tryCatch({
    cbind(
      package_name = 'CodelistGenerator',
      CodelistGenerator::benchmarkCodelistGenerator(cdm)
    )
  }, error = function(e) {
    ParallelLogger::logError("Execution of CodelistGenerator Benchmark failed: ", e)
    NULL
  })

  ParallelLogger::logInfo("> (3/4) IncidencePrevalence Benchmark")
  ipBenchmarkResult <- tryCatch({
    cbind(
      package_name = 'IncidencePrevalence',
      IncidencePrevalence::benchmarkIncidencePrevalence(cdm)
    )
  }, error = function(e) {
    ParallelLogger::logError("Execution of IncidencePrevalence Benchmark failed: ", e)
    NULL
  })

  ParallelLogger::logInfo("> (4/4) CohortCharacterisation Benchmark")
  # TODO: combine with CohortBenchmark above, applying to the first with count > 0
  ccBenchmarkResult <- tryCatch({
    cdm <- CDMConnector::generateConceptCohortSet(
      cdm = cdm,
      conceptSet = list(sinusitis = 40481087, pharyngitis = 4112343),
      name = "my_cohort"
    )
    cbind(
      package_name = 'CohortCharacteristics',
      CohortCharacteristics::benchmarkCohortCharacteristics(cdm$my_cohort)
    )
  }, error = function(e) {
    ParallelLogger::logError("Execution of CohortCharacteristics Benchmark failed: ", e)
    NULL
  })

  # Combine in one resultsObject and Return result with duration
  list(
    result = rbind(duBenchmarkResult, cgBenchmarkResult, ipBenchmarkResult, ccBenchmarkResult), 
    duration = as.numeric(difftime(Sys.time(), start_time), units = "secs")
  )
}
