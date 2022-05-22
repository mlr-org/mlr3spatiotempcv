# fs::dir_create("paper/figs")
knitr::purl(input = "paper/paper.Rmd", output = "paper/code.R", quiet = TRUE, documentation = 0)

# add call to install suggested packages
content = readLines("paper/code.R")
content[6] = "if (!requireNamespace(c('blockCV'), quietly = TRUE)) install.packages(c('blockCV', 'sf', 'patchwork', 'sperrorest', 'ggtext', 'plotly'))"

# remove knitr header
content = content[6:length(content)]
# remove all commented lines
content_no_knitr = content[!grepl("knitr::", content)]
content_no_comments = content_no_knitr[!grepl("#", content_no_knitr)]
writeLines(content_no_comments, "paper/article.R")

source("paper/article.R", chdir = TRUE)
# knitr::spin("paper/code.R")
# fs::file_move("code.md", "paper/code.md")
# fs::file_delete(c("code.md", "paper/code.R", "code.html"))
