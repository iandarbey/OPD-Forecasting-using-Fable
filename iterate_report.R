library(knitr)
library(rmarkdown)

load("ggplots.RData")

lapply(listforplotting, function(x) {
  rmarkdown::render(input = "rmarkdown.Rmd",
                    params = listforplotting,
                    output_format = "html_document",
                    output_file = paste("OPD_Report_on_", x, ".html", sep=''),
                    output_dir = "OPDReports//")
})
