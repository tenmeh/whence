# Build the live demo of the pkgdown site with shinylive.
#
# Run from the root of the package, after the site is built:
#
#   Rscript pkgdown/live-demo.R [site directory]
#
# The site directory is "docs" if you do not give one. The demo goes in
# <site>/demo, and the "Live demo" article shows it in an iframe.
#
# Shinylive runs the app in webR, R in the browser. webR installs packages
# only from a repository of WebAssembly builds, and whence is not in the
# default one. But whence is pure R: it has no compiled code. So an
# installed copy of whence goes into the app itself, and the app adds it to
# its library paths. The demo thus always uses the whence of this commit.
# The other packages come from the webR repository.
#
# shinylive finds the packages to bundle with renv::dependencies(), which
# reads the code of the app for library() and pkg:: calls. renv never reads
# a folder named "vendor". So the demo and whence go in vendor/, app.R at
# the top only sources them, and live-packages.R names the packages to
# bundle. reactable gives the table of "View the rows".
#
#   app/
#     app.R                  sources vendor/demo/app.R
#     live-packages.R        library() calls, for renv to read
#     vendor/demo/app.R      a copy of inst/examples/demo/app.R
#     vendor/lib/whence/     whence, installed from this commit

live_packages <- c("shiny", "dplyr", "reactable")

args <- commandArgs(trailingOnly = TRUE)
site <- if (length(args) >= 1L) args[[1]] else "docs"
if (!dir.exists(site)) {
  stop("The site directory `", site, "` does not exist. Build the site first.", call. = FALSE)
}

# --- The demo and whence, in vendor/ ------------------------------------------

app <- file.path(tempfile("whence-demo-"), "app")
demo <- file.path(app, "vendor", "demo")
lib <- file.path(app, "vendor", "lib")
dir.create(demo, recursive = TRUE)
dir.create(lib, recursive = TRUE)
invisible(file.copy(list.files("inst/examples/demo", full.names = TRUE), demo))

# --no-test-load: the check that the package loads runs in a new R, which
# does not know this library path yet.
status <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-test-load",
    paste0("--library=", shQuote(lib)), ".")
)
if (status != 0L || !file.exists(file.path(lib, "whence", "DESCRIPTION"))) {
  stop("R CMD INSTALL of whence into the demo failed.", call. = FALSE)
}

# --- The files at the top of the app ------------------------------------------

writeLines(c(
  "# Live demo. The demo is in vendor/demo, and whence is in vendor/lib.",
  "# See pkgdown/live-demo.R in the source of whence.",
  ".libPaths(c(normalizePath(\"vendor/lib\"), .libPaths()))",
  "source(\"vendor/demo/app.R\", local = TRUE)$value"
), file.path(app, "app.R"))

writeLines(c(
  "# The packages that shinylive bundles for the live demo. This file is not",
  "# run: renv::dependencies() only reads it.",
  sprintf("library(%s)", live_packages)
), file.path(app, "live-packages.R"))

# --- Export -------------------------------------------------------------------

out <- file.path(site, "demo")
unlink(out, recursive = TRUE)
shinylive::export(app, out, quiet = TRUE)
message("Live demo written to: ", out)
