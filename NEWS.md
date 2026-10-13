# whence 0.0.0.9000

* First version.

* `whenceOutput()` and `renderWhence()` show a value in 'shiny'.
  A click on the value opens a panel with the attrition waterfall and the
  reproduce code, with no trip to the server. `format` in
  `renderWhence()` turns the value into text, and a formatter of
  'scales' works there.

* `whence_code()` gives code that makes the value again. Only `input$name`
  and `input[["name"]]` are replaced by their values.

* `whence_rows()` finds the source rows that make the value. It runs only
  when it is called.

* `whence_trace()` splits a pipeline into its steps and records the rows
  after each step. When the chain stops at a call that whence does not know,
  such as a join, the trace and the panel say where. It works with the native pipe, the magrittr pipe and
  nested calls. `pivot_longer()` and `pivot_wider()` from 'tidyr' are known
  steps.
