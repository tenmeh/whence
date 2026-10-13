# A session that only records the custom messages that it is sent, with the
# userData that the group cache needs.
fake_table_session <- function() {
  sent <- list()
  list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1]] <<- list(type = type, message = message)
    },
    userData = list2env(list(.whence_groups = new.env())),
    sent = function() sent
  )
}

region_table <- function() {
  sales <- make_sales()
  whence_table(sales |> filter(year >= 2023) |> group_by(region) |> summarise(total = sum(revenue)))
}

test_that("the table payload holds the cells, the keys and the panel of the table", {
  local_dplyr()

  payload <- table_payload(region_table(), list(total = function(x) paste0("$", x)))

  expect_identical(payload$kind, "table")
  expect_identical(payload$columns, c("region", "total"))
  expect_identical(payload$rows[[1]], list("APAC", "$3900"))
  expect_identical(payload$explainable, TRUE)
  expect_identical(payload$panel$value, "2 rows")
})

test_that("format for a table is NULL, a function or a named list of functions", {
  expect_no_error(check_table_format(NULL))
  expect_no_error(check_table_format(toupper))
  expect_no_error(check_table_format(list(total = toupper)))
  expect_error(check_table_format(list(toupper)), "named list of functions")
  expect_error(check_table_format("dollar"), "named list of functions")
  expect_error(renderWhenceTable(1, format = 1), "named list of functions")
})

test_that("a function format changes number columns that are not group columns", {
  cells <- format_cells(data.frame(g = 1:2, x = c(1.5, 2.5), s = c("a", "b")), function(x) "N", keys = "g")

  expect_identical(cells, list(g = c("1", "2"), x = c("N", "N"), s = c("a", "b")))
})

test_that("a group request sends the panel of that row", {
  local_dplyr()
  traces <- list2env(list(tab = region_table()))
  session <- fake_table_session()

  handle_request(session, traces, list(id = "tab", action = "group", row = 2))

  sent <- session$sent()[[1]]
  expect_identical(sent$type, "whence:group")
  expect_identical(sent$message$row, 2L)
  expect_identical(sent$message$panel$value, "region = EMEA")
  expect_identical(sent$message$panel$rowText, "total = 9,900")
  expect_identical(ls(session$userData$.whence_groups), "tab\0372")
})

test_that("a removed request sends the rows that a step removed", {
  local_dplyr()
  traces <- list2env(list(tab = region_table()))
  session <- fake_table_session()
  local_mocked_bindings(send_rows = function(session, name, x, ..., step, row, title) {
    session$sendCustomMessage("whence:rows", list(rows = nrow(rows_query(x, step)$rows), title = title, row = row))
  })

  handle_request(session, traces, list(id = "tab", action = "removed", step = 1, row = 1))
  handle_request(session, traces, list(id = "tab", action = "removed", step = 9))

  sent <- session$sent()
  expect_identical(sent[[1]]$message$rows, 4L)
  expect_identical(sent[[1]]$message$title, "removed by year >= 2023")
  expect_identical(sent[[1]]$message$row, 1L)
  expect_identical(sent[[2]]$message$ok, FALSE)
})

test_that("a group request for a value, or for a row that is not there, fails clearly", {
  local_dplyr()
  sales <- make_sales()
  traces <- list2env(list(
    value = whence_trace(sales |> summarise(n = n())),
    tab = region_table()
  ))
  session <- fake_table_session()

  handle_request(session, traces, list(id = "value", action = "group", row = 1))
  handle_request(session, traces, list(id = "tab", action = "group", row = 7))

  messages <- lapply(session$sent(), function(x) x$message$message)
  expect_identical(unlist(messages), rep("This row cannot be explained.", 2))
})

test_that("a click on a discrete axis finds the row of its bar", {
  data <- data.frame(region = c("APAC", "EMEA"), total = c(3900, 9900))
  click <- function(x, y) {
    list(x = x, y = y, mapping = list(x = "region", y = "total"),
         domain = list(discrete_limits = list(x = list("APAC", "EMEA"))))
  }

  expect_identical(clicked_row(data, click(1.2, 100)), 1L)
  expect_identical(clicked_row(data, click(1.8, 100)), 2L)
  expect_identical(clicked_row(data, click(3, 100)), NA_integer_)
  expect_identical(clicked_row(data, list(x = 1, mapping = list(x = "nope"))), NA_integer_)
})

test_that("on a stacked bar, the click picks the row nearest in y", {
  data <- data.frame(year = c("2024", "2024"), type = c("a", "b"), n = c(10, 30))
  click <- list(x = 1, y = 28, mapping = list(x = "year", y = "n"),
                domain = list(discrete_limits = list(x = list("2024"))))

  expect_identical(clicked_row(data, click), 2L)
})

test_that("a point request with a click off the bars says so", {
  local_dplyr()
  traces <- list2env(list(tab = region_table()))
  session <- fake_table_session()
  off <- list(x = 5, y = 0, mapping = list(x = "region", y = "total"),
              domain = list(discrete_limits = list(x = list("APAC", "EMEA"))))

  handle_request(session, traces, list(id = "tab", action = "point", click = off))
  handle_request(session, traces, list(id = "tab", action = "point", click = utils::modifyList(off, list(x = 1))))

  sent <- session$sent()
  expect_identical(sent[[1]]$message$message, "Click on a bar or a point of the plot.")
  expect_identical(sent[[2]]$message$panel$value, "region = APAC")
})

test_that("a new render forgets the panels of the rows of the old table", {
  local_dplyr()
  sales <- make_sales()
  server <- function(input, output, session) {
    output$tab <- renderWhenceTable(
      sales |> filter(year >= 2023) |> group_by(region) |> summarise(total = sum(revenue))
    )
  }

  shiny::testServer(server, {
    expect_identical(output$tab$kind, "table")
    handle_request(session, session$userData$.whence_traces, list(id = "tab", action = "group", row = 1))
    expect_length(ls(session$userData$.whence_groups), 1)
    remember_trace(session, "tab", region_table())
    expect_length(ls(session$userData$.whence_groups), 0)
  })
})

test_that("renderWhencePlot() checks plot", {
  expect_error(renderWhencePlot(1, plot = "bars"), "`plot` must be a function")
})
