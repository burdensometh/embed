test_that("numeric outcome is encoded using the hierarchy", {
  dat <- make_hier_data()

  rec <- hier_recipe(dat, product, category, department) |> prep()

  expect_equal(
    names(rec$steps[[1]]$mapping$tables),
    c("product", "category", "department")
  )
  expect_equal(rec$steps[[1]]$mapping$finest, "product")

  baked <- bake(rec, new_data = dat)
  expect_equal(names(baked), c("outcome", "hier_product"))
  expect_type(baked$hier_product, "double")

  # The finest level is used when it was seen during training.
  td <- tidy(rec, number = 1)
  product_a <- td$value[td$terms == "product" & td$level == "a"]
  expect_equal(baked$hier_product[dat$product == "a"], rep(product_a, 2))
})

test_that("parents are used for smoothing", {
  dat <- make_hier_data_nested()

  rec <- hier_recipe(dat, product, category, department) |> prep()
  td <- tidy(rec, number = 1)

  # Product "a" has five observations with mean 10, pulled towards its parent
  # "c1" whose mean is 5. With kappa = 10 and n = 5 the pooled value is
  # (5 * 10 + 10 * parent) / 15.
  parent_c1 <- td$value[td$terms == "category" & td$level == "c1"]
  expect_equal(
    td$value[td$terms == "product" & td$level == "a"],
    (5 * 10 + 10 * parent_c1) / 15
  )

  category_c1 <- td$value[td$terms == "category" & td$level == "c1"]
  category_c2 <- td$value[td$terms == "category" & td$level == "c2"]
  expect_gt(abs(category_c1 - category_c2), 1e-8)
})

test_that("smooth = FALSE uses unpooled estimates", {
  dat <- make_hier_data_nested()

  rec <- hier_recipe(dat, product, category, department, smooth = FALSE) |>
    prep()
  td <- tidy(rec, number = 1)

  expect_equal(
    td$value[td$terms == "product" & td$level == "a"],
    10
  )
  expect_equal(
    td$value[td$terms == "category" & td$level == "c1"],
    5
  )
})

test_that("unseen levels fall back to the finest available parent", {
  dat <- make_hier_data()
  rec <- hier_recipe(dat, product, category, department) |> prep()
  td <- tidy(rec, number = 1)

  value_x <- td$value[td$terms == "category" & td$level == "x"]
  value_y <- td$value[td$terms == "category" & td$level == "y"]
  value_global <- td$value[td$terms == "product" & td$level == "..new"]

  new_dat <- data.frame(
    product = c("unseen1", "unseen2", "unseen3"),
    category = c("x", "y", "unseen"),
    department = c("d1", "d2", "d3")
  )

  baked <- bake(rec, new_data = new_dat)$hier_product

  expect_equal(baked[1], value_x)
  expect_equal(baked[2], value_y)
  expect_equal(baked[3], value_global)

  # An unseen product does not collapse to the global value when its parent is
  # known.
  expect_false(isTRUE(all.equal(value_x, value_global)))
})

test_that("missing finest values are encoded separately from unseen levels", {
  dat <- make_hier_data()
  dat$product[1:2] <- NA

  rec <- hier_recipe(dat, product, category, department) |> prep()
  td <- tidy(rec, number = 1)

  missing_value <- td$value[td$terms == "product" & td$level == "..missing"]
  unseen_value <- td$value[td$terms == "product" & td$level == "..new"]

  expect_false(is.na(missing_value))
  expect_gt(abs(missing_value - unseen_value), 1e-8)

  new_dat <- data.frame(
    product = c(NA, "unseen"),
    category = c("x", "unseen"),
    department = c("d1", "unseen")
  )

  baked <- bake(rec, new_data = new_dat)$hier_product
  expect_equal(baked[1], missing_value)
  expect_equal(baked[2], unseen_value)
})

test_that("missing parents are skipped while falling back", {
  dat <- make_hier_data()
  rec <- hier_recipe(dat, product, category, department) |> prep()
  td <- tidy(rec, number = 1)

  value_d2 <- td$value[td$terms == "department" & td$level == "d2"]

  new_dat <- data.frame(
    product = "unseen",
    category = NA_character_,
    department = "d2"
  )

  expect_equal(bake(rec, new_data = new_dat)$hier_product, value_d2)
})

test_that("levels with only missing outcomes are treated as unseen", {
  dat <- data.frame(
    outcome = c(1, 2, 3, 4, NA, NA),
    product = c("a", "a", "b", "b", "e", "e"),
    category = c("x", "x", "y", "y", "x", "x")
  )

  rec <- hier_recipe(dat, product, category) |> prep()
  td <- tidy(rec, number = 1)

  expect_false("e" %in% td$level[td$terms == "product"])

  # The parent of "e" is known, so it falls back to that parent's estimate.
  value_x <- td$value[td$terms == "category" & td$level == "x"]
  expect_equal(
    bake(rec, data.frame(product = "e", category = "x"))$hier_product,
    value_x
  )
})

test_that("a single level is shrunk towards the global estimate", {
  dat <- make_hier_data()
  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(product, outcome = vars(outcome)) |>
    prep()

  td <- tidy(rec, number = 1)
  global <- td$value[td$level == "..new"]

  expect_equal(global, mean(dat$outcome))
  # With kappa = 10 and two observations per product, a product's estimate is
  # 2/12 of its raw mean and 10/12 of the global mean.
  raw_a <- mean(dat$outcome[dat$product == "a"])
  expect_equal(
    td$value[td$level == "a"],
    (2 * raw_a + 10 * global) / 12
  )
})

test_that("parent statistics only use the training outcome", {
  dat <- make_hier_data()
  rec <- hier_recipe(dat, product, category, department) |> prep()

  new_dat <- dat
  new_dat$outcome <- rev(new_dat$outcome)

  expect_equal(
    bake(rec, new_data = new_dat)$hier_product,
    bake(rec, new_data = dat)$hier_product
  )
})

test_that("case weights are used", {
  dat <- make_hier_data()
  dat$wts <- importance_weights(c(1, 3, 1, 3, 1, 3, 1, 3))

  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(product, category, department, outcome = vars(outcome)) |>
    prep()

  expect_true(rec$steps[[1]]$case_weights)

  td <- tidy(rec, number = 1)
  parent_x <- td$value[td$terms == "category" & td$level == "x"]

  # Product "a" is the weighted mean of outcomes 1 and 2 with weights 1 and 3,
  # and its support is the sum of those weights; it is pooled towards its
  # parent.
  expect_equal(
    td$value[td$terms == "product" & td$level == "a"],
    (4 * 1.75 + 10 * parent_x) / 14
  )

  unweighted <- hier_recipe(
    dat[setdiff(names(dat), "wts")],
    product,
    category,
    department
  ) |>
    prep()
  expect_false(identical(tidy(unweighted, 1)$value, td$value))
})

test_that("nominal outcomes are encoded as log odds", {
  dat <- make_hier_data()
  dat$outcome <- factor(
    ifelse(dat$outcome > 4, "yes", "no"),
    levels = c("yes", "no")
  )

  rec <- hier_recipe(dat, product, category, department) |> prep()
  td <- tidy(rec, number = 1)

  # Product "a" has only one outcome class; its log odds are made finite.
  value <- td$value[td$terms == "product" & td$level == "a"]
  expect_true(is.finite(value))
})

test_that("character and factor outcomes agree", {
  dat <- make_hier_data()
  dat$outcome <- ifelse(dat$outcome > 4, "yes", "no")

  rec_chr <- hier_recipe(dat, product, category, department) |> prep()

  dat$outcome <- factor(dat$outcome)
  rec_fct <- hier_recipe(dat, product, category, department) |> prep()

  expect_equal(
    tidy(rec_chr, 1)$value,
    tidy(rec_fct, 1)$value
  )
})

test_that("hierarchy can be supplied as a formula or vars()", {
  dat <- make_hier_data()

  by_dots <- hier_recipe(dat, product, category, department) |> prep()

  by_formula <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(
      product ~ category + department,
      outcome = vars(outcome)
    ) |>
    prep()

  by_vars <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(
      hierarchy = vars(product, category, department),
      outcome = vars(outcome)
    ) |>
    prep()

  expect_equal(
    tidy(by_dots, 1)[c("terms", "level", "value", "n", "parent")],
    tidy(by_formula, 1)[c("terms", "level", "value", "n", "parent")]
  )
  expect_equal(tidy(by_dots, 1)$value, tidy(by_vars, 1)$value)
})

test_that("contradictory parent relations are rejected", {
  dat <- make_hier_data()
  dat$category <- rep(c("x", "y"), length.out = 8)

  expect_snapshot(
    error = TRUE,
    hier_recipe(dat, product, category, department) |> prep()
  )
})

test_that("isolated children are rejected", {
  dat <- make_hier_data()
  dat$category[7:8] <- NA

  expect_snapshot(
    error = TRUE,
    hier_recipe(dat, product, category, department) |> prep()
  )
})

test_that("contradictory parent relations are rejected during bake", {
  dat <- make_hier_data()
  rec <- hier_recipe(dat, product, category, department) |> prep()

  new_dat <- data.frame(
    product = "a",
    category = "y",
    department = "d1"
  )

  expect_snapshot(
    error = TRUE,
    bake(rec, new_data = new_dat)
  )
})

test_that("the new column is used by later steps", {
  dat <- make_hier_data()

  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(product, category, department, outcome = vars(outcome)) |>
    step_normalize(all_predictors()) |>
    prep()

  baked <- bake(rec, new_data = dat)

  expect_equal(names(baked), c("outcome", "hier_product"))
  expect_equal(mean(baked$hier_product), 0)

  info <- rec$term_info
  expect_equal(
    info$role[info$variable == "hier_product"],
    "predictor"
  )
  expect_equal(
    info$source[info$variable == "hier_product"],
    "derived"
  )
})

test_that("role argument is respected", {
  dat <- make_hier_data()

  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(
      product, category, department,
      role = "id",
      outcome = vars(outcome)
    ) |>
    prep()

  info <- rec$term_info
  expect_equal(info$role[info$variable == "hier_product"], "id")
})

test_that("keep_original_cols works", {
  dat <- make_hier_data()

  rec <- hier_recipe(dat, product, category, department) |> prep()
  expect_equal(names(bake(rec, dat)), c("outcome", "hier_product"))

  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(
      product, category, department,
      outcome = vars(outcome),
      keep_original_cols = TRUE
    ) |>
    prep()
  expect_equal(
    names(bake(rec, dat)),
    c("product", "category", "department", "outcome", "hier_product")
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  dat <- make_hier_data()

  rec <- hier_recipe(dat, product, category, department)
  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(rec <- prep(rec))
  expect_no_error(bake(rec, new_data = dat))
})

test_that("prefix controls the new column name", {
  dat <- make_hier_data()
  rec <- hier_recipe(
    dat,
    product, category, department,
    prefix = "dept"
  ) |>
    prep()

  expect_equal(names(bake(rec, dat)), c("outcome", "dept_product"))
})

test_that("the alias step_lencode_hierarchical() works", {
  dat <- make_hier_data()

  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hierarchical(
      product, category, department,
      outcome = vars(outcome)
    ) |>
    prep()

  expect_equal(names(bake(rec, dat)), c("outcome", "hier_product"))

  expect_equal(
    tidy(rec, 1)$value,
    tidy(hier_recipe(dat, product, category, department) |> prep(), 1)$value
  )
})

test_that("required_pkgs is correct", {
  rec <- recipe(outcome ~ ., data = make_hier_data()) |>
    step_lencode_hier(product, category, department, outcome = vars(outcome))

  expect_equal(required_pkgs(rec), c("recipes", "embed"))
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  dat <- make_hier_data()

  rec <- hier_recipe(dat, product, category, department) |>
    update_role(department, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = dat, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = dat[, -4])
  )
})

test_that("empty printing", {
  rec <- recipe(outcome ~ ., data = make_hier_data())
  rec <- step_lencode_hier(rec, outcome = vars(outcome))

  expect_snapshot(rec)

  rec <- prep(rec, make_hier_data())

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  dat <- make_hier_data()
  rec1 <- recipe(outcome ~ ., data = dat)
  rec2 <- step_lencode_hier(rec1, outcome = vars(outcome))

  rec1 <- prep(rec1, dat)
  rec2 <- prep(rec2, dat)

  expect_identical(bake(rec1, dat), bake(rec2, dat))
})

test_that("empty selection tidy method works", {
  rec <- recipe(outcome ~ ., data = make_hier_data())
  rec <- step_lencode_hier(rec, outcome = vars(outcome))

  expect <- tibble(
    terms = character(),
    level = character(),
    value = double(),
    n = integer(),
    parent = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, make_hier_data())

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  dat <- make_hier_data()
  rec <- recipe(outcome ~ ., data = dat) |>
    step_lencode_hier(product, category, department, outcome = vars(outcome))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
