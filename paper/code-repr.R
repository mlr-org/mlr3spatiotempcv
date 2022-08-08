# fs::dir_create("paper/figs")
knitr::purl(input = "paper/article.Rmd", output = "paper/code.R", quiet = TRUE, documentation = 0)

# add call to install suggested packages
content = readLines("paper/code.R")
content[6] = "if (!requireNamespace(c('blockCV'), quietly = TRUE)) install.packages(c('blockCV', 'sf', 'patchwork', 'sperrorest', 'ggtext', 'plotly'))"
content[7] = "dir.create('pdf')"

# remove knitr header
content = content[6:length(content)]
# remove all commented lines
content_no_comments = content[!grepl("#", content)]
writeLines(content_no_comments, "paper/code.R")

source("paper/code.R", chdir = TRUE)
styler.mlr::style_file("paper/code.R")

knitr::spin("paper/code.R", knit = FALSE)
rmarkdown::render("paper/code.Rmd")
