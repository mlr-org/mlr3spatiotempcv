# fs::dir_create("paper/figs")
knitr::purl(input = "paper/article.Rmd", output = "paper/code.R", quiet = TRUE, documentation = 0)

# add call to install suggested packages
content = readLines("paper/code.R")
content[8] = "if (!requireNamespace(c('blockCV'), quietly = TRUE)) install.packages(c('blockCV', 'sf', 'patchwork', 'sperrorest', 'ggtext', 'plotly', 'mlr3', 'mlr3spatiotempcv', 'knitr', 'rmarkdown', 'terra', 'reticulate'))"
content[9] = "dir.create('pdf')"

# remove knitr header
content = content[6:length(content)]
# remove all commented lines
content_no_comments = content[!grepl("#", content)]

# add custom content
content_no_comments[1] = "# The following lines install dependencies for the plotly-based 3D plots. If you already have a working plotly/python/reticulate installation, these commands DO NOT need to be executed\n# install.packages('reticulate')\n# reticulate::install_miniconda()\n# reticulate::use_miniconda('r-reticulate')\n# reticulate::conda_install('r-reticulate', 'python-kaleido')\n# reticulate::conda_install('r-reticulate', 'plotly')\n\n# R package dependencies"

writeLines(content_no_comments, "paper/code.R")

source("paper/code.R", chdir = TRUE)
styler.mlr::style_file("paper/code.R")

knitr::spin("paper/code.R", knit = FALSE)
rmarkdown::render("paper/code.Rmd")
