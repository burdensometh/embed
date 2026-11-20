library(testthat)
library(dplyr)

# Hand-checkable example: with num_breaks = 8, each of the 8 observations
# falls in its own fine bin (fine breaks are 1, 1.875, 2.75, ..., 8) and the
# event rates per fine bin are 0, 1, 0, 1, 1, 0, 1, 1.
sim8 <- tibble(
  x = 1:8,
  class = factor(
    c("no", "yes", "no", "yes", "yes", "no", "yes", "yes"),
    levels = c("yes", "no")
  )
)

sim8_dec <- tibble(
  x = 1:8,
  class = factor(
    c("yes", "yes", "yes", "no", "yes", "no", "no", "no"),
    levels = c("yes", "no")
  )
)

test_that("low-level binning merges adjacent bins with PAV", {
  expect_identical(
    embed:::monotonic_binning(
      sim8$x,
      "x",
      sim8$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "increasing"
    ),
    c(1.875, 3.625, 6.25, 7.125)
  )
})

test_that("low-level binning enforces min_weight", {
  # each fine bin has weight 1 < 2, so light bins are pooled with the
  # closest-rate neighbor before the monotonic merging
  expect_identical(
    embed:::monotonic_binning(
      sim8$x,
      "x",
      sim8$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 2,
      mono = "increasing"
    ),
    c(3.625, 6.25)
  )
})

test_that("low-level binning respects the direction", {
  # event rates per fine bin: 1, 1, 1, 0, 1, 0, 0, 0
  expected <- c(1.875, 2.75, 3.625, 5.375, 6.25, 7.125)
  expect_identical(
    embed:::monotonic_binning(
      sim8_dec$x,
      "x",
      sim8_dec$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "decreasing"
    ),
    expected
  )
  # auto picks up the decreasing direction
  expect_identical(
    embed:::monotonic_binning(
      sim8_dec$x,
      "x",
      sim8_dec$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "auto"
    ),
    expected
  )
  # forcing the opposite direction pools everything into a single bin
  expect_identical(
    embed:::monotonic_binning(
      sim8_dec$x,
      "x",
      sim8_dec$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "increasing"
    ),
    numeric(0)
  )
})

test_that("event_level picks the positive class", {
  # "no" is the second level: event rates 1, 0, 1, 0, 0, 1, 0, 0
  expect_identical(
    embed:::monotonic_binning(
      sim8$x,
      "x",
      sim8$class,
      event_level = "second",
      num_breaks = 8,
      min_weight = 0,
      mono = "decreasing"
    ),
    c(1.875, 3.625, 6.25, 7.125)
  )
})

test_that("character outcomes work", {
  expect_identical(
    embed:::monotonic_binning(
      sim8$x,
      "x",
      as.character(sim8$class),
      event_level = "second",
      num_breaks = 8,
      min_weight = 0,
      mono = "increasing"
    ),
    c(1.875, 3.625, 6.25, 7.125)
  )
})

test_that("constant predictors are not binned", {
  expect_snapshot(
    splits <- embed:::monotonic_binning(
      rep(1, 8),
      "x",
      sim8$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "auto"
    )
  )
  expect_identical(splits, numeric(0))
})

test_that("binary predictors get a single trivial split", {
  expect_identical(
    embed:::monotonic_binning(
      rep(0:1, each = 4),
      "x",
      sim8$class,
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "auto"
    ),
    0.5
  )
})

test_that("pav_blocks pools adjacent violators, respecting weights", {
  unweighted <- embed:::pav_blocks(c(0.4, 0.3, 0.35), c(1, 1, 1))
  expect_identical(unweighted$end, c(2L, 3L))
  expect_equal(unweighted$rate, c(0.35, 0.35))

  # heavy first block pulls the pooled rate above the last block
  weighted <- embed:::pav_blocks(c(0.4, 0.3, 0.35), c(100, 1, 1))
  expect_identical(weighted$end, 3L)
})

test_that("merge_min_weight pools light bins with the closest-rate neighbor", {
  res <- embed:::merge_min_weight(
    w_bin = c(1, 10, 1, 10),
    e_bin = c(0, 5, 1, 2),
    bin_end = 2:5,
    min_weight = 5
  )
  expect_identical(res$weight, c(12, 10))
  expect_identical(res$events, c(6, 2))
  expect_identical(res$end, 4:5)

  # min_weight of 0 is a no-op
  res0 <- embed:::merge_min_weight(
    w_bin = c(1, 10, 1, 10),
    e_bin = c(0, 5, 1, 2),
    bin_end = 2:5,
    min_weight = 0
  )
  expect_identical(res0$weight, c(1, 10, 1, 10))
  expect_identical(res0$end, 2:5)
})

test_that("step function for classification", {
  expect_snapshot({
    mono_rec <-
      recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x, outcome = "class", num_breaks = 8, min_weight = 0) |>
      prep()
  })

  expect_identical(names(mono_rec$steps[[1]]$rules), "x")
  expect_identical(mono_rec$steps[[1]]$rules$x, c(1.875, 3.625, 6.25, 7.125))

  expect_no_error(
    mono_pred <- bake(mono_rec, sim8)
  )

  # a real ordered factor with stable levels
  expect_true(is.ordered(mono_pred$x))
  expect_identical(
    levels(mono_pred$x),
    levels(cut(
      sim8$x,
      c(-Inf, 1.875, 3.625, 6.25, 7.125, Inf),
      include.lowest = TRUE,
      right = FALSE,
      dig.lab = 4
    ))
  )
  expect_identical(as.integer(mono_pred$x), c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 5L))
})

test_that("bake applies training cut points to new data", {
  mono_rec <-
    recipe(class ~ ., data = sim8) |>
    step_discretize_monotonic(x, outcome = "class", num_breaks = 8, min_weight = 0) |>
    prep()

  tidy_before <- tidy(mono_rec, number = 1)

  # out-of-range values fall into the first/last bin
  new <- tibble(
    x = c(-100, 2, 7.5, 100),
    class = sim8$class[1:4]
  )
  baked_new <- bake(mono_rec, new)
  expect_identical(as.integer(baked_new$x), c(1L, 2L, 5L, 5L))

  # levels are stable across data sets, even when some bins are empty
  expect_identical(levels(baked_new$x), levels(bake(mono_rec, sim8)$x))

  # boundaries are not re-estimated on the new data
  expect_identical(tidy(mono_rec, number = 1), tidy_before)
})

test_that("missing values are handled separately", {
  sim8_na <- sim8
  sim8_na$x[1] <- NA
  sim8_na$class[8] <- NA

  mono_rec <-
    recipe(class ~ ., data = sim8_na) |>
    step_discretize_monotonic(x, outcome = "class", num_breaks = 8, min_weight = 0) |>
    prep()

  # missing values are excluded when estimating the cut points
  expect_identical(
    mono_rec$steps[[1]]$rules$x,
    embed:::monotonic_binning(
      sim8$x[2:7],
      "x",
      sim8$class[2:7],
      event_level = "first",
      num_breaks = 8,
      min_weight = 0,
      mono = "auto"
    )
  )

  # and stay missing in the baked data
  baked_na <- bake(mono_rec, sim8_na)
  expect_true(is.na(baked_na$x[1]))
  expect_false(any(is.na(baked_na$x[-1])))
})

test_that("event rates are monotonic across the baked bins", {
  set.seed(20240)
  n <- 3000
  sim <- tibble(x = runif(n), z = runif(n))
  sim$class <- factor(
    ifelse(rbinom(n, 1, plogis(-4 + 8 * sim$x)) == 1, "yes", "no"),
    levels = c("yes", "no")
  )

  bin_rates <- function(rec, data, outcome) {
    rules <- rec$steps[[1]]$rules$x
    binned <- cut(
      data$x,
      c(-Inf, rules, Inf),
      include.lowest = TRUE,
      right = FALSE
    )
    tapply(outcome == "yes", binned, mean)
  }

  # increasing relationship, direction picked up automatically
  rec <- recipe(class ~ ., data = sim) |>
    step_discretize_monotonic(x, outcome = "class") |>
    prep()
  rates <- bin_rates(rec, sim, sim$class)
  expect_true(length(rates) > 1)
  expect_true(all(diff(rates) >= 0))

  # cut points are real, strictly increasing boundaries
  rules <- rec$steps[[1]]$rules$x
  expect_true(all(diff(rules) > 0))
  expect_true(all(rules > min(sim$x) & rules < max(sim$x)))

  # decreasing relationship
  sim$class <- factor(
    ifelse(rbinom(n, 1, plogis(4 - 8 * sim$x)) == 1, "yes", "no"),
    levels = c("yes", "no")
  )
  rec_dec <- recipe(class ~ ., data = sim) |>
    step_discretize_monotonic(x, outcome = "class") |>
    prep()
  rates_dec <- bin_rates(rec_dec, sim, sim$class)
  expect_true(length(rates_dec) > 1)
  expect_true(all(diff(rates_dec) <= 0))

  # the requested direction is enforced even against the data
  rec_forced <- recipe(class ~ ., data = sim) |>
    step_discretize_monotonic(x, outcome = "class", mono = "increasing") |>
    prep()
  rates_forced <- bin_rates(rec_forced, sim, sim$class)
  expect_true(all(diff(rates_forced) >= 0))
})

test_that("binned variables feed into step_woe()", {
  set.seed(1)
  n <- 2000
  sim_tr <- tibble(x = runif(n), z = runif(n))
  sim_tr$class <- factor(
    ifelse(rbinom(n, 1, plogis(-3 + 6 * sim_tr$x)) == 1, "yes", "no"),
    levels = c("yes", "no")
  )
  sim_te <- tibble(x = runif(500), z = runif(500))
  sim_te$class <- factor(
    ifelse(rbinom(500, 1, plogis(-3 + 6 * sim_te$x)) == 1, "yes", "no"),
    levels = c("yes", "no")
  )

  rec <- recipe(class ~ ., data = sim_tr) |>
    step_discretize_monotonic(x, outcome = "class") |>
    step_woe(x, outcome = vars(class))

  expect_no_error(rec <- prep(rec))

  baked <- bake(rec, sim_te)
  expect_true("woe_x" %in% names(baked))
  expect_true(is.numeric(baked$woe_x))
  expect_false(any(is.na(baked$woe_x)))
})

test_that("case weights are used", {
  # event rates per value of x: 0.4, 0.3, 0.35
  sim_w <- tibble(
    x = c(rep(1, 5), rep(2, 10), rep(3, 20)),
    class = factor(
      c(
        rep(c("yes", "no"), c(2, 3)),
        rep(c("yes", "no"), c(3, 7)),
        rep(c("yes", "no"), c(7, 13))
      ),
      levels = c("yes", "no")
    )
  )
  wts <- c(rep(20, 5), rep(0.1, 10), rep(0.05, 20))

  rec_unw <-
    recipe(class ~ ., data = sim_w) |>
    step_discretize_monotonic(
      x,
      outcome = "class",
      num_breaks = 100,
      min_weight = 0,
      mono = "increasing"
    ) |>
    prep()
  expect_identical(names(rec_unw$steps[[1]]$rules), "x")
  expect_length(rec_unw$steps[[1]]$rules$x, 1)

  # with weights, the heavy first bin pools everything into a single bin
  expect_snapshot({
    rec_w <-
      recipe(class ~ ., data = sim_w |> mutate(w = importance_weights(wts))) |>
      step_discretize_monotonic(
        x,
        outcome = "class",
        num_breaks = 100,
        min_weight = 0,
        mono = "increasing"
      ) |>
      prep()
  })
  expect_length(rec_w$steps[[1]]$rules, 0)
  expect_true(rec_w$steps[[1]]$case_weights)

  expect_snapshot(rec_w)
})

test_that("tidy method", {
  mono_rec <-
    recipe(class ~ ., data = sim8) |>
    step_discretize_monotonic(x, outcome = "class", num_breaks = 8, min_weight = 0)

  res <- tidy(mono_rec, number = 1)
  expect_equal(res$terms, "x")
  expect_equal(res$value, NA_real_)

  mono_rec <- prep(mono_rec)

  res <- tidy(mono_rec, number = 1)
  expect_equal(res$terms, rep("x", 4))
  expect_equal(res$value, c(1.875, 3.625, 6.25, 7.125))
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x, outcome = "class", num_breaks = 1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x, outcome = "class", num_breaks = 2.5) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x, outcome = "class", min_weight = -1) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x, outcome = "class", mono = "sideways") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x, outcome = "class", event_level = "third") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8) |>
      step_discretize_monotonic(x)
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8 |> mutate(class = 1:8)) |>
      step_discretize_monotonic(x, outcome = "class") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8 |>
             mutate(class = factor(rep(letters[1:3], length.out = 8)))) |>
      step_discretize_monotonic(x, outcome = "class") |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim8 |> mutate(ch = letters[1:8])) |>
      step_discretize_monotonic(x, ch, outcome = "class") |>
      prep()
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) |>
    step_discretize_monotonic(all_predictors(), outcome = "mpg")
  rec_param <- tunable.step_discretize_monotonic(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_breaks", "min_weight"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ ., data = sim8) |>
    step_discretize_monotonic(x, outcome = "class", num_breaks = 8, min_weight = 0) |>
    update_role(x, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  expect_snapshot(
    rec_trained <- prep(rec, training = sim8, verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = sim8[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_discretize_monotonic(rec, outcome = "mpg")

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_discretize_monotonic(rec1, outcome = "mpg")

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_discretize_monotonic(rec, outcome = "mpg")

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(class ~ ., data = sim8) |>
    step_discretize_monotonic(x, outcome = "class", num_breaks = 8, min_weight = 0)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <-
    recipe(~., data = mtcars) |>
    step_discretize_monotonic(
      all_predictors(),
      outcome = "mpg",
      num_breaks = hardhat::tune(),
      min_weight = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
