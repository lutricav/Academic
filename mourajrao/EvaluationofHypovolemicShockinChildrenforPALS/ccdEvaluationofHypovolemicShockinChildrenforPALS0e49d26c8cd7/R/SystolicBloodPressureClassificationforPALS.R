# Copyright 2015, Timothy W. Cook <tim@mlhim.org>
# Licensed under the Apache License, Version 2.0 (the 'License');
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an 'AS IS' BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#' \code{Systolic Blood Pressure Classification for PALS.}
#'
#' Returns a data.frame of the collected nodes of: \code{Systolic Blood Pressure Classification for PALS} from the XML instance passed as fileName.
#' The source name and the CCD ID are also included in each row to assist in identifying the source of the data.
#' The XML element name is pcs-0a0aea7f-fece-4eff-ba3f-0b1828fbef8d as a restriction of the DvOrdinal
#' The vectors are: label, vtb, vte, #' normal_status, ordinal, symbol, rr_label, rr_definition, rr_vtb, rr_vtb, rr_is_normal, interval_label, interval_lower, interval_upper, lower_included, upper_included, lower_bounded, upper_bounded, #' ccd, sourceName,
#' 
#' Systolic Blood Pressure Classification for PALS
#' 
#' @references
#' The data is structured according to the Multi-Level Healthcare Information Modelling Reference Model release 2.5.0
#' \url{http://www.mlhim.org}
#' The semantic reference(s) for this data:
#' @references
#' 
#' @param fileList - The path/file name(s) of the XML file(s) to process.
#' @return A dataframe consisting of the vectors listed in the Description.
#' 
#' The examples use the included files. Any source that can be processed via the XML::xmlTreeParse function can be used.
#' @examples
#' files <- dir('./inst/examples', recursive=TRUE, full.names=TRUE, pattern='\\.xml$')
#' SystolicBloodPressureClassificationforPALS <- getSystolicBloodPressureClassificationforPALS(files) 
#' 
#' @export
getSystolicBloodPressureClassificationforPALS <- function(sourceList)
{
    data <- lapply(sourceList, parseSystolicBloodPressureClassificationforPALS)
    data <- data.table::rbindlist(data, fill=TRUE)
    return(data)
}

#' @export
parseSystolicBloodPressureClassificationforPALS <- function(sourceName) {
  nsDEF <- c(mlhim2='http://www.mlhim.org/ns/mlhim2/', xsi='http://www.w3.org/2001/XMLSchema-instance')
  doc <- XML::xmlTreeParse(sourceName, handlers=list('comment'=function(x,...){NULL}), asTree = TRUE)
  root <- XML::xmlRoot(doc)
  pcm <- XML::getNodeSet(root, '//mlhim2:pcs-0a0aea7f-fece-4eff-ba3f-0b1828fbef8d', nsDEF)
  data <- lapply(pcm, mlhim250rm::DvOrdinal)
  data <- data.table::rbindlist(data, fill=TRUE)
  if (length(data) > 0) { 
      data$ccd <- XML::xmlName(root)
      data$pcs <- 'pcs-0a0aea7f-fece-4eff-ba3f-0b1828fbef8d'
      data$ccd <- XML::xmlName(root)
      data$sourceName <- sourceName
 } else {
    data <- data.frame()
  }
 return(data)
}
