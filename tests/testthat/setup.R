if (Sys.getenv("CI") == "true") {
  if (Sys.info()["sysname"] != "Darwin") {
    source("https://gist.githubusercontent.com/pat-s/6430470cf817050e27d26c43c0e9be72/raw/c90480893fdd57ce990795a702aada41382e03a9/install-cluto.R") # nolint
  }
}
