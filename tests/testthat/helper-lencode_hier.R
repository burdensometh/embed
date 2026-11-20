make_hier_data <- function() {
  data.frame(
    outcome = c(1, 2, 3, 4, 5, 6, 7, 8),
    product = c("a", "a", "b", "b", "c", "c", "d", "d"),
    category = c("x", "x", "x", "x", "y", "y", "y", "y"),
    department = c("d1", "d1", "d1", "d1", "d2", "d2", "d2", "d2")
  )
}

# Each product has five observations, all with the same outcome, so that the
# unpooled product and category means are easy to reason about.
make_hier_data_nested <- function() {
  n <- 40
  data.frame(
    department = rep(c("d1", "d2"), each = n / 2),
    category = rep(c("c1", "c2", "c3", "c4"), each = n / 4),
    product = rep(letters[1:8], each = n / 8),
    outcome = c(
      rep(10, 5),
      rep(0, 5),
      rep(20, 5),
      rep(0, 5),
      rep(5, 5),
      rep(0, 5),
      rep(15, 5),
      rep(0, 5)
    )
  )
}

hier_recipe <- function(data, ..., smooth = TRUE, prefix = "hier") {
  recipe(outcome ~ ., data = data) |>
    step_lencode_hier(
      ...,
      outcome = vars(outcome),
      smooth = smooth,
      prefix = prefix
    )
}
