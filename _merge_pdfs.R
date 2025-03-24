#' combine rendered report with cover (and back cover)
#'
#' this script adds the cover page to the pdf version of the final report

library(pdftools)

pdf_combine(c(
    "./figures/Brazil-Europe-2019-2023-Cover.pdf"
  , "./docs/Operational-Comparison-of-ANS-Performance-in-Brazil-and-Europe.pdf"), 
            output = "report-with-cover.pdf")