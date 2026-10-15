# CRAN comments

## Submission

This is a new submission. The package has not been on CRAN before.

## Test environments

* local Windows 11, R 4.6.1 (release), `R CMD check --as-cran` on a tarball
  built from a clean export of the commit, with `_R_CHECK_CRAN_INCOMING_=TRUE`,
  `_R_CHECK_CRAN_INCOMING_REMOTE_=TRUE`, the PDF manual, and pandoc. Run once
  without `NOT_CRAN` and once with it.
* GitHub Actions with `--as-cran`: macOS (release), Windows (release),
  Ubuntu (devel, release, oldrel-1 to oldrel-4, and R 4.1).

## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer: 'Tanmay Chanda <tanmaychanda96@gmail.com>'
  New submission

The only note is the standard first-submission note.

## Notes for the reviewer

* The examples of `whenceOutput()`, `whenceTableOutput()` and
  `whencePlotOutput()` start a 'shiny' app, so they are wrapped in
  `if (interactive())`. The parts that need no app run in the examples: each
  output function, and the whole engine (`whence_trace()`, `whence_table()`,
  `whence_group()`, `whence_rows()`), which works in the console.
* The tests that start a headless browser (with 'shinytest2') call
  `skip_on_cran()`, and each one stops its app and its browser when it ends.
* `whence_rows()` runs the steps of a pipeline a second time to find rows.
  When a step is `slice_sample()`, it sets `.Random.seed` to the state that
  the first run used, so that the same rows are drawn, and puts back the
  user's state at the end with `on.exit()`. It changes nothing else in the
  global environment.
