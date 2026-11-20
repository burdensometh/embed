#' Discretize numeric variables with monotonic binning
#'
#' `step_discretize_monotonic()` creates a *specification* of a recipe step
#' that will discretize numeric data (e.g. integers or doubles) into bins in
#' a  supervised way, such that the event rate of a binary outcome is
#' monotonic across the bins.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [recipes::selections] for more details.
#' @param role Defaults to `"predictor"`.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome. The outcome must have exactly 2 classes.
#' @param event_level A single string, either `"first"` (the default) or
#'   `"second"`, to indicate which level of the outcome is the event (the
#'   "positive" class) whose rate is made monotonic.
#' @param num_breaks The number of fine-grained bins each variable is
#'   pre-binned into before the monotonic merging. Defaults to 20.
#' @param min_weight The minimum total case weight allowed in a single bin.
#'   Bins with less weight are pooled with a neighboring bin before the
#'   monotonic merging. When case weights are not used, this is the number of
#'   data points in a bin. Defaults to 20.
#' @param mono A single string. Should the event rate be `"increasing"` or
#'   `"decreasing"` as the variable increases, or `"auto"` (the default) to
#'   pick the direction that best matches the data?
#' @param rules The cut points defining the bins for each variable. If length
#'   zero, the variable could not be binned.
#' @param id A character string that is unique to this step to identify it.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'   by [recipes::bake()]? While all operations are baked when
#'   [recipes::prep()] is run, some operations may not be able to be conducted
#'   on new data (e.g. processing the outcome variable(s)). Care should be
#'   taken when using `skip = TRUE` as it may affect the computations for
#'   subsequent operations.
#' @template step-return
#' @details
#'
#' `step_discretize_monotonic()` creates non-uniform bins from numerical
#' variables by utilizing the information about a binary outcome, such that
#' the event rate is monotonic (either increasing or decreasing) as the
#' variable increases. This is a common requirement in credit scoring and
#' other regulated modeling, where the bins are typically converted to
#' weights of evidence with [step_woe()].
#'
#' For each variable, the training data is first split into `num_breaks`
#' fine-grained bins. Bins that contain less than `min_weight` total case
#' weight are then pooled with the neighboring bin with the most similar
#' event rate. Finally, adjacent bins are merged with the pool adjacent
#' violators algorithm (the weighted isotonic regression of the bin event
#' rates) until the event rates are monotonic, and the boundaries of the
#' merged bins become the cut points of the discretization.
#'
#' The cut points are estimated on the training data only; [recipes::bake()]
#' applies the same cut points to new data. Values outside of the range of
#' the training data are assigned to the first or last bin. Missing values
#' are not used when estimating the cut points and stay missing in the baked
#' data; use an imputation step or [recipes::step_unknown()] beforehand if a
#' missing value bin is desired.
#'
#' The binned variables are returned as ordered factors whose levels are the
#' same for every data set, which makes the output suitable for further
#' processing with [step_woe()].
#'
#' Note that the original data will be replaced with the new bins.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned
#' with columns `terms`, `value`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, location of the cut points}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_discretize_monotonic"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-supervised
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(modeldata)
#' data(credit_data)
#' library(rsample)
#'
#' set.seed(1234)
#' split <- initial_split(credit_data, strata = "Status")
#'
#' credit_tr <- training(split)
#' credit_te <- testing(split)
#'
#' mono_rec <-
#'   recipe(Status ~ Income + Assets, data = credit_tr) |>
#'   step_discretize_monotonic(Income, Assets, outcome = "Status")
#'
#' mono_rec <- prep(mono_rec, training = credit_tr)
#'
#' # The cut points:
#' tidy(mono_rec, number = 1)
#'
#' # Binned values are ordered factors and can be passed to step_woe():
#' bake(mono_rec, credit_te, Income)
#' @seealso [recipes::step_discretize()], [step_discretize_cart()],
#'   [step_discretize_xgb()], [step_woe()]
#' @export
step_discretize_monotonic <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    outcome = NULL,
    event_level = c("first", "second"),
    num_breaks = 20,
    min_weight = 20,
    mono = c("auto", "increasing", "decreasing"),
    rules = NULL,
    skip = FALSE,
    id = rand_id("discretize_monotonic")
  ) {
    if (is.null(outcome)) {
      cli::cli_abort(
        "The {.arg outcome} argument should select at least one column."
      )
    }

    event_level <- rlang::arg_match0(event_level, c("first", "second"))
    mono <- rlang::arg_match0(mono, c("auto", "increasing", "decreasing"))

    add_step(
      recipe,
      step_discretize_monotonic_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        outcome = outcome,
        event_level = event_level,
        num_breaks = num_breaks,
        min_weight = min_weight,
        mono = mono,
        rules = rules,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }

step_discretize_monotonic_new <-
  function(
    terms,
    role,
    trained,
    outcome,
    event_level,
    num_breaks,
    min_weight,
    mono,
    rules,
    skip,
    id,
    case_weights
  ) {
    step(
      subclass = "discretize_monotonic",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      event_level = event_level,
      num_breaks = num_breaks,
      min_weight = min_weight,
      mono = mono,
      rules = rules,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

monotonic_binning <- function(
  predictor,
  term,
  outcome,
  event_level = "first",
  num_breaks = 20,
  min_weight = 20,
  mono = "auto",
  wts = NULL
) {
  if (is.null(wts)) {
    wts <- rep(1, length(predictor))
  }
  wts <- as.double(wts)

  if (is.factor(outcome)) {
    outcome_levels <- levels(outcome)
  } else {
    outcome_levels <- sort(unique(outcome[!is.na(outcome)]))
  }
  event <- outcome_levels[[if (event_level == "first") 1L else 2L]]

  # Missing values are handled separately: they are excluded when the cut
  # points are estimated and stay missing when the data is baked.
  keep <- !is.na(predictor) & !is.na(outcome) & wts > 0
  x <- predictor[keep]
  y <- as.double(outcome[keep] == event)
  w <- as.double(wts[keep])

  if (length(unique(x)) < 2) {
    cli::cli_warn(
      "{.fn step_discretize_monotonic} failed to find any meaningful splits
      for predictor {.val {term}}, which will not be binned."
    )
    return(numeric(0))
  }

  # Fine-grained pre-binning of the training data
  fine_breaks <- unique(
    stats::quantile(x, probs = seq(0, 1, length.out = num_breaks + 1), names = FALSE)
  )
  n_fine <- length(fine_breaks) - 1L
  if (n_fine < 2L) {
    return(numeric(0))
  }

  fine_bin <- findInterval(x, fine_breaks, rightmost.closed = TRUE)
  w_bin <- tapply(w, fine_bin, sum)
  e_bin <- tapply(w * y, fine_bin, sum)
  # Index of the upper break (in `fine_breaks`) of each observed fine bin
  bin_end <- as.integer(names(w_bin)) + 1L
  w_bin <- as.numeric(w_bin)
  e_bin <- as.numeric(e_bin)

  if (mono == "auto") {
    mid <- (fine_breaks[bin_end - 1L] + fine_breaks[bin_end]) / 2
    mono <- if (weighted_cov(mid, e_bin / w_bin, w_bin) < 0) {
      "decreasing"
    } else {
      "increasing"
    }
  }

  # Pool bins that are too light, then merge adjacent bins until the event
  # rates are monotonic
  merged <- merge_min_weight(w_bin, e_bin, bin_end, min_weight)

  rate <- merged$events / merged$weight
  if (mono == "decreasing") {
    rate <- -rate
  }
  blocks <- pav_blocks(rate, merged$weight)

  # Rebuild the actual cut points from the pooled blocks
  boundaries <- fine_breaks[merged$end[blocks$end]]
  unname(boundaries[-length(boundaries)])
}

# Weighted pool adjacent violators algorithm: fits the weighted isotonic
# regression (non-decreasing) of `rate` over ordered groups and returns the
# pooled blocks.
pav_blocks <- function(rate, weight) {
  n <- length(rate)
  blk_rate <- numeric(n)
  blk_weight <- numeric(n)
  blk_end <- integer(n)
  n_blocks <- 0L
  for (i in seq_len(n)) {
    n_blocks <- n_blocks + 1L
    blk_rate[n_blocks] <- rate[i]
    blk_weight[n_blocks] <- weight[i]
    blk_end[n_blocks] <- i
    while (n_blocks > 1L && blk_rate[n_blocks - 1L] > blk_rate[n_blocks]) {
      pool_weight <- blk_weight[n_blocks - 1L] + blk_weight[n_blocks]
      blk_rate[n_blocks - 1L] <-
        (blk_rate[n_blocks - 1L] * blk_weight[n_blocks - 1L] +
          blk_rate[n_blocks] * blk_weight[n_blocks]) /
          pool_weight
      blk_weight[n_blocks - 1L] <- pool_weight
      blk_end[n_blocks - 1L] <- blk_end[n_blocks]
      n_blocks <- n_blocks - 1L
    }
  }
  list(
    rate = blk_rate[seq_len(n_blocks)],
    weight = blk_weight[seq_len(n_blocks)],
    end = blk_end[seq_len(n_blocks)]
  )
}

# Iteratively pool the lightest bin (below `min_weight`) with the adjacent
# bin whose event rate is closest. Bins stay contiguous and ordered.
merge_min_weight <- function(w_bin, e_bin, bin_end, min_weight) {
  while (length(w_bin) > 1L && any(w_bin < min_weight)) {
    under <- which(w_bin < min_weight)
    i <- under[which.min(w_bin[under])]
    rate <- e_bin / w_bin
    if (i == 1L) {
      j <- 2L
    } else if (i == length(w_bin)) {
      j <- i - 1L
    } else if (abs(rate[i] - rate[i - 1L]) <= abs(rate[i] - rate[i + 1L])) {
      j <- i - 1L
    } else {
      j <- i + 1L
    }
    lo <- min(i, j)
    hi <- max(i, j)
    w_bin[lo] <- w_bin[lo] + w_bin[hi]
    e_bin[lo] <- e_bin[lo] + e_bin[hi]
    bin_end[lo] <- bin_end[hi]
    w_bin <- w_bin[-hi]
    e_bin <- e_bin[-hi]
    bin_end <- bin_end[-hi]
  }
  list(weight = w_bin, events = e_bin, end = bin_end)
}

weighted_cov <- function(x, y, w) {
  w_sum <- sum(w)
  x_bar <- sum(w * x) / w_sum
  y_bar <- sum(w * y) / w_sum
  sum(w * (x - x_bar) * (y - y_bar)) / w_sum
}

#' @export
prep.step_discretize_monotonic <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_number_whole(x$num_breaks, min = 2, arg = "num_breaks")
  check_number_decimal(x$min_weight, min = 0, arg = "min_weight")
  x$mono <- rlang::arg_match0(x$mono, c("auto", "increasing", "decreasing"))
  x$event_level <- rlang::arg_match0(x$event_level, c("first", "second"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used)) {
    wts <- rep(1, nrow(training))
  }

  if (length(col_names) > 0) {
    check_type(training[, col_names], types = c("double", "integer"))

    y_name <- recipes_eval_select(x$outcome, training, info)

    col_names <- col_names[col_names != y_name]

    outcome <- training[[y_name]]
    if (is.factor(outcome)) {
      n_classes <- length(levels(outcome))
    } else if (is.character(outcome)) {
      n_classes <- length(unique(outcome[!is.na(outcome)]))
    } else {
      cli::cli_abort(
        "{.arg outcome} must select a factor or character column, not
        {.obj_type_friendly {outcome}}."
      )
    }
    if (n_classes != 2L) {
      cli::cli_abort(
        "{.arg outcome} must select a column with exactly 2 classes, not
        {n_classes}."
      )
    }

    rules <-
      purrr::map2(
        training[, col_names],
        col_names,
        monotonic_binning,
        outcome = outcome,
        event_level = x$event_level,
        num_breaks = x$num_breaks,
        min_weight = x$min_weight,
        mono = x$mono,
        wts = wts
      )

    has_splits <- purrr::map_lgl(rules, \(.x) length(.x) > 0)

    rules <- rules[has_splits]
    col_names <- col_names[has_splits]
    if (length(col_names) > 0) {
      names(rules) <- col_names
    }
  } else {
    rules <- list()
  }

  step_discretize_monotonic_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    event_level = x$event_level,
    num_breaks = x$num_breaks,
    min_weight = x$min_weight,
    mono = x$mono,
    rules = rules,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_discretize_monotonic <- function(object, new_data, ...) {
  vars <- object$rules
  check_new_data(names(vars), object, new_data)

  for (i in seq_along(vars)) {
    if (length(vars[[i]]) > 0) {
      var <- names(vars)[[i]]
      binned_data <- new_data

      binned_data[[var]] <- cut(
        new_data[[var]],
        breaks = c(-Inf, object$rules[[i]], Inf),
        include.lowest = TRUE,
        right = FALSE,
        ordered_result = TRUE,
        dig.lab = 4
      )

      recipes::check_name(binned_data, new_data, object)
      new_data <- binned_data
    }
  }
  new_data
}

#' @export
print.step_discretize_monotonic <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Discretizing variables using monotonic binning "
  print_step(
    names(x$rules),
    x$terms,
    x$trained,
    title,
    width,
    case_weights = x$case_weights
  )
  invisible(x)
}

#' @rdname step_discretize_monotonic
#' @usage NULL
#' @export
tidy.step_discretize_monotonic <- function(x, ...) {
  if (is_trained(x)) {
    num_splits <- purrr::map_int(x$rules, length)

    if (length(num_splits) > 0) {
      res <- tibble(
        terms = rep(names(x$rules), num_splits),
        value = unlist(x$rules, use.names = FALSE)
      )
    } else {
      res <- tibble(
        terms = character(),
        value = double()
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = NA_real_
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_discretize_monotonic <- function(x, ...) {
  c("embed")
}

#' @export
#' @rdname tunable_embed
tunable.step_discretize_monotonic <- function(x, ...) {
  tibble::tibble(
    name = c("num_breaks", "min_weight"),
    call_info = list(
      list(pkg = "dials", fun = "num_breaks"),
      list(pkg = "dials", fun = "min_n")
    ),
    source = "recipe",
    component = "step_discretize_monotonic",
    component_id = x$id
  )
}
