# Source:
# https:/www.statsandr.com/blog/a-package-to-download-free-springer-books-during-covid-19-quarantine/

# Acknolwedge: All credits go to the source above.

# Library
# devtools::install_github("renanxcortes/springerQuarantineBooksR")
library(springerQuarantineBooksR)

springer_table <- download_springer_table()

library(DT)
springer_table$open_url <- paste0(
  '<a target="_blank" href="', # open HTML tag
  springer_table$open_url, # write out the website link
  '">SpringerLink</a>' # close HTML tag
)


springer_table <- springer_table[, c(1:3, 19, 20)]
datatable(
  springer_table,
  rownames = FALSE, # hide row names
  filter = "top", # exhibit filter function
  extensions = "Buttons", # present button function
  options = list(
    autoWidth = TRUE,
    dom = "Blfrtip",
    buttons = c("copy", "csv", "excel", "pdf", "print"),
    pageLength = 5,
    order = list(0, "asc")
  ),
  escape = FALSE
)

download_springer_book_files()
