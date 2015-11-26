# Copyright 2013-2015, Timothy W. Cook <tim@mlhim.org>
# metadata.R for ccd-6ae55d46-7f07-400f-b43b-0e49d26c8cd7.xsd
# Licensed under the Apache License, Version 2.0 (the 'License');
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an 'AS IS' BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#' @title getMetadata
#'
#' The Concept Constraint Definition (CCD) Metadata
#' @export
getMetadata <- data.frame(
  dc_title='Evaluation of Hypovolemic Shock in Children for PALS',
  dc_creator='Arovel Oliveira Moura Junior',
  dc_contributors='None',
  dc_subject='Decision Support Systems, Clinical; Emergency Medical Services; Maternal and Child Health',
  dc_source='PALS',
  dc_relation='None',
  dc_coverage='Pediatrics',
  dc_type='MLHIM Concept Constraint Definition (CCD)',
  dc_identifier='ccd-6ae55d46-7f07-400f-b43b-0e49d26c8cd7',
  dc_description='Evaluation of Hypovolemic Shock in Children for the Pediatric Advanced Life Support guideline',
  dc_publisher='MLHIM Laboratory',
  dc_date=as.POSIXct(strptime('2015-11-26 21:3716', '%Y-%m-%d %HH:%MM%SS')),
  dc_format='text/xml',
  dc_language='en-us',
  stringsAsFactors=FALSE)
  
ccduri <- function(){
    return('http://www.ccdgen.com/ccdlib/ccd-6ae55d46-7f07-400f-b43b-0e49d26c8cd7.xsd')
}

