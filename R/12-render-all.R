render_html <- function(filename) { 
  rmarkdown::render(input = paste0("./R/", filename, ".R"), 
                    output_file = paste0("./notebooks/", filename, ".html"), 
                    knit_root_dir = ".")
}
render_pdf <- function(filename) { 
  rmarkdown::render(input = paste0("./R/", filename, ".R"), 
                    output_file = paste0("./notebooks/", filename, ".pdf"), 
                    knit_root_dir = ".")
}
render_html("01-load-packages")
render_html("02-scrape-bitcoin-data")
render_html("03-scrape-google-data")
render_html("04-scrape-other-data")
render_html("05-engineer-features")
render_html("06-plot-data")
render_html("07-cross-validate-model")
render_html("08-train-model")
render_html("09-evaluate-model")
render_html("10-explore-google-trends")
render_html("11-explore-wikipedia")
