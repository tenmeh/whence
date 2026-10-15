# whence 0.1.0

* Initial CRAN submission.

* `whenceOutput()` and `renderWhence()` show a value in 'shiny'. A click on
  the value opens a panel with the attrition waterfall and the reproduce
  code, with no trip to the server. `format` in `renderWhence()` turns the
  value into text, and a formatter of 'scales' works there.

* `whenceTableOutput()` and `renderWhenceTable()` show a summary table. A
  click on a row opens the panel of that row: the rows of its group at each
  step, the code of the row, and its rows.

* `whencePlotOutput()` and `renderWhencePlot()` draw a 'ggplot2' plot of a
  summary. A click on a bar or a point opens the panel of its row, also when
  the axis is computed, as in `aes(reorder(region, total), total)`.

* `whence_code()` gives code that makes the value again. Only `input$name`
  and `input[["name"]]` are replaced by their values.

* `whence_rows()` finds the source rows that make the value. With `step`,
  it finds the rows that one step removed. It runs only when it is called.
  In the panel, each count of removed rows, with its percentage, is a
  button that shows them. `print()` and the panel show the number of each
  step.

* `whence_table()` and `whence_group()` trace a summary table and explain
  one of its rows, with no 'shiny'. `whence_group()` finds the row by its
  number, or by its group values, such as `whence_group(tab, region = "APAC")`.

* `whence_opened()` gives a reactive with the panel that the viewer has open,
  and the row of the table, so an app can follow the viewer.

* `whence_trace()` splits a pipeline into its steps and records the rows
  after each step. When the chain stops at a call that whence does not know,
  such as a join, the trace and the panel say where. `pivot_longer()` and
  `pivot_wider()` from 'tidyr' are known steps.
