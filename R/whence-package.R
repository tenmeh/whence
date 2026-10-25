#' @keywords internal
#' @aliases whence-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# 'dtplyr' runs its data.table code in the environment of the caller. In the
# count pass that is this namespace, and data.table only treats `dt[...]` as
# its own syntax for a package that says it is aware of it. whence does not
# import data.table.
.datatable.aware <- TRUE # nolint: object_name_linter. data.table reads this name.
