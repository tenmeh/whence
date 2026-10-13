# whence <img src="man/figures/logo.png" align="right" height="139" alt="whence logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/tenmeh/whence/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tenmeh/whence/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Click a number in a 'shiny' dashboard and see where its data went.

`whence` answers one question: *"Why is this number smaller than I
expected?"* The answer is an attrition waterfall. It shows how many rows
each step of the pipeline removed, and gives the code that makes the number
again.

```
$4,281,330

  Where the rows went
    sales                 18,442 rows
      region == "APAC"     3,180 rows   -15,262 rows
      year >= 2023         1,204 rows    -1,976 rows
      aggregated to 1 row      1 row

  Reproduce                                        [Copy]
    sales |>
      filter(region == "APAC") |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue))

  [View the rows]
```

## Limits

Read these first.

- **Database-backed pipelines are not supported.** With 'dbplyr', the steps
  become SQL and nothing runs until `collect()`. There are thus no row
  counts to show for each step. `whence` stops with an error for a
  `tbl_lazy`, and does not show a waterfall that would mislead. This
  excludes many large dashboards.
- **Joins are not traced.** A join ends the chain. The join and everything
  upstream of it become the source row of the waterfall.
- **Window functions** (a `mutate()` after `group_by()` that reads other
  rows) are counted correctly. Their row attribution has no clear meaning.
- The verbs of 'data.table', 'arrow' and 'duckplyr' are not known.
- One value for each output. Tables and plots are not explained.

## Installation

```r
# install.packages("pak")
pak::pak("tenmeh/whence")
```

## Usage

Replace `textOutput()` and `renderText()` with `explainableOutput()` and
`renderExplainable()`. Give the render a pipeline that ends in one value.
You do not need to rewrite it.

```r
library(shiny)
library(dplyr)
library(whence)

ui <- fluidPage(
  selectInput("region", "Region", c("APAC", "EMEA")),
  explainableOutput(
    "total",
    label = "Revenue",
    format = function(x) paste0("$", format(x, big.mark = ","))
  )
)

server <- function(input, output, session) {
  output$total <- renderExplainable(
    sales |>
      filter(region == input$region) |>
      filter(year >= 2023) |>
      summarise(total = sum(revenue))
  )
}
```

The viewer clicks the value, or presses Enter on it, and a panel opens. The
panel holds the waterfall and the code. It opens with no trip to the server.
"View the rows" asks the server for the rows that make the value.

The demo has a few outputs to try. It also runs in your browser, with no
install, on the [live demo](https://tenmeh.github.io/whence/articles/live-demo.html)
page. To run it on your machine:

```r
shiny::runApp(system.file("examples", "demo", package = "whence"))
```

## With no 'shiny'

The engine works on its own:

```r
trace <- whence_trace(
  mtcars |>
    filter(cyl == 4) |>
    filter(mpg > 25) |>
    summarise(mean_hp = mean(hp))
)

trace                 # the waterfall, as text
as.data.frame(trace)  # one row for each step
whence_value(trace)   # the value
whence_code(trace)    # the reproduce code
whence_rows(trace)    # the rows of mtcars that make the value
```

## How it works

The native pipe leaves no trace in the parsed code: `x |> f() |> g()` is
`g(f(x))`. `whence` thus knows a list of verbs, such as `filter()`,
`mutate()`, `summarise()` and `slice_max()`. For a call to one of these, the
first argument is the data from upstream. The magrittr pipe `%>%` works too.

Each step runs once, in order, in the environment of the render. It reads
the same inputs as the full expression would. This is the same work as
running the pipeline in one piece.

A summary step is not attrition. When `summarise()` turns 1,204 rows into
one, the waterfall says "aggregated to 1 row", and not "-1,203".

The rows that make the value are found only when the viewer asks for them.
The steps then run again on a copy of the source with a hidden id column.
After `distinct()`, `pivot_longer()`, `pivot_wider()` or a step that is not
a known verb, the link to the source rows is lost. The panel then says so,
and the waterfall still works.

In the reproduce code, only `input$name` and `input[["name"]]` are replaced
by their values. Each other name is written as it is, because a wider rule
could replace a name that a verb reads as a column.

## Related packages

- 'lineager' tags rows with lasting ids that survive joins, for audit. It is
  a tool for the analyst. `whence` is a tool for the viewer of a dashboard:
  one click, no change to the pipeline, counts and not identities.
- 'dplyneage' traces the lineage of columns.
