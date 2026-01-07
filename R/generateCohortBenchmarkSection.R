# @file generateDedSection.R
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

#' Generates the Cohort Benchmark section for the Results Document
#'
#' @param doc officer document object to add the section to
#' @param df Results object from \code{cdmOnboarding}
generateCohortBenchmarkSection <- function(doc, df) {
  dfPretty <- df %>%
    dplyr::mutate(
      `Cohort` = .data$cohort_name,
      `#Persons` = ifelse(is.na(.data$error), .data$n_subject_bins, 'ERROR'),
      `Duration` = ifelse(is.na(.data$error), prettyunits::pretty_sec(.data$duration), 'ERROR'),
      .keep = "none"
    )
  
  doc <- doc %>%
    my_table_caption('Results from generating cohort benchmark.',
      sourceSymbol = pkg.env$sources$cdm
    ) %>%
    my_body_add_table(dfPretty)

  if (all(is.na(df$error))) {
    return(doc)
  }

  doc <- doc %>%
    officer::body_add_break() %>%
    officer::body_add_par("Cohort Generation Errors", style = pkg.env$styles$highlight)
  
  # For each error add a note
  for (i in seq_len(nrow(df))) {
    if (is.na(df$error[i])) {
      next
    }
    doc <- doc %>%
      officer::body_add_par(sprintf(
          "Cohort '%s' could not be generated due to an error:",
          df$cohort_name[i]          
      )) %>%
      officer::body_add_par(
        df$error[i],
        style = pkg.env$styles$footnote
      ) %>%
      officer::body_add_par("")
  }

  return(doc)
}
