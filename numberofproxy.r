library(yaml)

files <- list.files(
#  "fillinpath",
  pattern = "\\.yml$",
  full.names = TRUE
)

has_proxy <- sapply(files, function(f) {
  y <- tryCatch(yaml.load_file(f), error = function(e) NULL)
  !is.null(y$tags) && "Proxy" %in% y$tags
})

proxy_files <- basename(files[has_proxy])

cat("Number of files with Proxy tag:", length(proxy_files), "\n")
print(proxy_files)