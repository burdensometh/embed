#' Hierarchical likelihood encoding
#'
#' `step_lencode_hier()` creates a *specification* of a recipe step that will
#' convert a nominal predictor into a numeric encoding that borrows information
#' from one or more parent predictors. A typical example is a product that
#' belongs to a category, which in turn belongs to a department.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose the variables that make
#'   up the hierarchy. The variables are ordered from the finest level to the
#'   coarsest level, e.g. `product, category, department`. See
#'   [recipes::selections()] for more details. A single formula, such as
#'   `product ~ category + department`, or a call to [dplyr::vars()] can be
#'   used instead. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the new column created by this step will be
#'   used as a predictor in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome. Only numeric and nominal outcomes are currently supported.
#' @param hierarchy An optional formula, character vector, or call to
#'   [dplyr::vars()] that describes the hierarchy from the finest level to the
#'   coarsest level. When `NULL`, the order of the variables selected by `...`
#'   is used.
#' @param smooth A logical. Should the estimates of levels with low support be
#'   pulled towards the estimate of their parent level? Defaults to `TRUE`. See
#'   Details.
#' @param prefix A character string that will be the prefix to the resulting new
#'   variable.
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `FALSE`.
#' @param mapping A list of tibble results that define the encoding. This is
#'   `NULL` until the step is trained by [recipes::prep()].
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [recipes::bake()]? While all operations are baked when [recipes::prep()] is
#'   run, some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using
#'   `skip = TRUE` as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms`, `level`, `value`, `n`, `parent`, and `id`.
#' @keywords datagen
#' @concept preprocessing encoding
#' @details
#'
#' The selected variables describe a hierarchy, with the first variable being
#' the finest level and the last variable being the coarsest level. For numeric
#' outcomes, a level is encoded by the mean outcome of the observations with
#' that level. For nominal outcomes, it is encoded by the log odds that the
#' first level of the outcome is present. Case weights, when used, are respected
#' by these summaries. All statistics are computed from the training outcome
#' only; the outcome of new data is never used.
#'
#' The step learns which value of each level belongs to which value of the next
#' coarser level from the training data. These relations, together with the
#' number of observations supporting each level, are stored in the trained step.
#'
#' When `smooth = TRUE`, the estimate for a level is partially pooled towards
#' the estimate of its parent:
#'
#' \deqn{
#' estimate = (s * raw + \kappa * parent) / (s + \kappa)
#' }
#'
#' where \eqn{s} is the support of the level (the number of observations, or
#' the sum of the case weights when they are used), \eqn{raw} is the unpooled
#' estimate, \eqn{parent} is the (already pooled) estimate of the next coarser
#' level, and \eqn{\kappa = 10}. The coarsest level is pooled towards the global
#' estimate. Setting `smooth = FALSE` uses the unpooled estimates.
#'
#' During [recipes::bake()], a row is encoded using the finest level that has a
#' usable estimate. If the finest value was observed during training, its
#' estimate is used. If it is unseen, the row falls back to the estimate of its
#' parent value in the new data, then to the next parent, and finally to the
#' global estimate. An unseen product therefore does not always fall back to a
#' single global value when its category or department is known.
#'
#' Missing values in the finest level are treated separately from unseen
#' categories. They receive an estimate computed from the training rows where
#' the finest level was missing, pooled towards the global estimate when
#' `smooth = TRUE`. This value is reported as `..missing` by [tidy()], while the
#' value used for unseen categories is reported as `..new`. Missing values in a
#' parent level are skipped when falling back through the hierarchy.
#'
#' Parent relations are validated during [recipes::prep()]. A value that is
#' associated with more than one value of its parent is contradictory, and a
#' value that never occurs together with a parent value is isolated; both cases
#' result in an error. Relations are also checked during [recipes::bake()], and
#' a row whose parent values contradict the relations learned during training is
#' rejected.
#'
#' The new variable is named `{prefix}_{finest level}`. The hierarchy variables
#' are removed unless `keep_original_cols = TRUE`.
#'
#' The step uses nested hierarchical smoothing, so no new mixed model
#' algorithm is required. `step_lencode_hierarchical()` is an alias for
#' `step_lencode_hier()`, and `step_lencode()` remains available for a single
#' predictor without a hierarchy.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned
#' with columns `terms`, `level`, `value`, `n`, `parent`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the level of the hierarchy}
#'   \item{level}{character, the values of the level}
#'   \item{value}{numeric, the encoding}
#'   \item{n}{integer, the number of training observations supporting the level}
#'   \item{parent}{character, the value of the next coarser level}
#'   \item{id}{character, id of this step}
#' }
#'
#' The rows with `level = "..new"` and `level = "..missing"` describe the
#' values used when the finest level is unseen or missing, respectively.
#'
#' @template case-weights-supervised
#'
#' @references
#'
#' Micci-Barreca D (2001) "A preprocessing scheme for high-cardinality
#' categorical attributes in classification and prediction problems," ACM SIGKDD
#' Explorations Newsletter, 3(1), 27-32.
#'
#' @examples
#' library(recipes)
#' library(dplyr)
#'
#' product_data <- data.frame(
#'   outcome = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   product = c("a", "a", "b", "b", "c", "c", "d", "d"),
#'   category = c("x", "x", "x", "x", "y", "y", "y", "y"),
#'   department = c("d1", "d1", "d1", "d1", "d2", "d2", "d2", "d2")
#' )
#'
#' rec <- recipe(outcome ~ ., data = product_data) |>
#'   step_lencode_hier(
#'     product, category, department,
#'     outcome = vars(outcome)
#'   ) |>
#'   prep()
#'
#' bake(rec, new_data = product_data)
#' tidy(rec, number = 1)
#' @seealso [step_lencode()], [step_lencode_glm()],
#'   [step_lencode_bayes()], [step_lencode_mixed()]
#' @export
step_lencode_hier <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  outcome = NULL,
  hierarchy = NULL,
  smooth = TRUE,
  prefix = "hier",
  keep_original_cols = FALSE,
  mapping = NULL,
  skip = FALSE,
  id = rand_id("lencode_hier")
) {
  lencode_hier_spec(
    recipe = recipe,
    subclass = "lencode_hier",
    terms = enquos(...),
    role = role,
    trained = trained,
    outcome = outcome,
    hierarchy = hierarchy,
    smooth = smooth,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    mapping = mapping,
    skip = skip,
    id = id
  )
}

#' @rdname step_lencode_hier
#' @export
step_lencode_hierarchical <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  outcome = NULL,
  hierarchy = NULL,
  smooth = TRUE,
  prefix = "hier",
  keep_original_cols = FALSE,
  mapping = NULL,
  skip = FALSE,
  id = rand_id("lencode_hier")
) {
  lencode_hier_spec(
    recipe = recipe,
    subclass = "lencode_hierarchical",
    terms = enquos(...),
    role = role,
    trained = trained,
    outcome = outcome,
    hierarchy = hierarchy,
    smooth = smooth,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    mapping = mapping,
    skip = skip,
    id = id
  )
}

lencode_hier_spec <- function(
  recipe,
  subclass,
  terms,
  role,
  trained,
  outcome,
  hierarchy,
  smooth,
  prefix,
  keep_original_cols,
  mapping,
  skip,
  id
) {
  if (is.null(outcome)) {
    cli::cli_abort(
      "Please list a variable in {.arg outcome}.",
      class = "embed_error_lencode_hier_outcome"
    )
  }

  check_string(prefix)
  check_bool(smooth)

  if (length(terms) > 0) {
    first <- quo_get_expr(terms[[1]])
    if (is_formula(first) || is_call(first, "vars")) {
      if (!is.null(hierarchy)) {
        cli::cli_abort(
          "The hierarchy can be specified either in {.arg ...} or in
          {.arg hierarchy}, not both.",
          class = "embed_error_lencode_hier_hierarchy"
        )
      }
      if (length(terms) > 1) {
        cli::cli_abort(
          "When a hierarchy is given to {.arg ...}, it must be the only
          argument.",
          class = "embed_error_lencode_hier_hierarchy"
        )
      }
      hierarchy <- eval_tidy(terms[[1]])
      terms <- quos()
    }
  }

  add_step(
    recipe,
    lencode_hier_new(
      subclass = subclass,
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      hierarchy = hierarchy,
      smooth = smooth,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      mapping = mapping,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

lencode_hier_new <- function(
  subclass,
  terms,
  role,
  trained,
  outcome,
  hierarchy,
  smooth,
  prefix,
  keep_original_cols,
  mapping,
  skip,
  id,
  case_weights
) {
  step(
    subclass = subclass,
    terms = terms,
    role = role,
    trained = trained,
    outcome = outcome,
    hierarchy = hierarchy,
    smooth = smooth,
    prefix = prefix,
    keep_original_cols = keep_original_cols,
    mapping = mapping,
    skip = skip,
    id = id,
    case_weights = case_weights
  )
}

#' @export
prep.step_lencode_hier <- function(x, training, info = NULL, ...) {
  lencode_hier_prep(x, training, info)
}

#' @export
prep.step_lencode_hierarchical <- function(x, training, info = NULL, ...) {
  lencode_hier_prep(x, training, info)
}

lencode_hier_prep <- function(x, training, info) {
  col_names <- recipes_eval_select(x$terms, training, info)

  hierarchy <- x$hierarchy
  if (is.null(hierarchy)) {
    levs <- col_names
  } else {
    if (length(col_names) > 0) {
      cli::cli_abort(
        "Select the hierarchy variables either in {.arg ...} or in
        {.arg hierarchy}, not both.",
        class = "embed_error_lencode_hier_hierarchy"
      )
    }
    levs <- hier_names(hierarchy, call = rlang::caller_env())
  }

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts)
  if (isFALSE(were_weights_used) || is.null(wts)) {
    wts <- NULL
  }

  if (length(levs) > 0) {
    levs <- recipes_eval_select(
      rlang::quos(!!!rlang::syms(unname(levs))),
      training,
      info
    )
    levs <- unname(levs)

    y_name <- recipes_eval_select(x$outcome, training, info)
    if (length(y_name) != 1) {
      cli::cli_abort(
        "{.arg outcome} must select exactly one variable.",
        class = "embed_error_lencode_hier_outcome"
      )
    }
    if (y_name %in% levs) {
      cli::cli_abort(
        "The outcome {.var {y_name}} cannot be part of the hierarchy.",
        class = "embed_error_lencode_hier_hierarchy"
      )
    }

    check_type(training[, levs], types = c("string", "factor", "ordered"))

    mapping <- lencode_hier_calc(
      training = training,
      levels = levs,
      y = training[[y_name]],
      wts = wts,
      smooth = x$smooth
    )
  } else {
    mapping <- list()
  }

  lencode_hier_new(
    subclass = lencode_hier_subclass(x),
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    hierarchy = levs,
    smooth = x$smooth,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    mapping = mapping,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

lencode_hier_subclass <- function(x) {
  cls <- class(x)[1]
  sub("^step_", "", cls)
}

hier_names <- function(x, call = rlang::caller_env()) {
  if (is_formula(x)) {
    out <- c(all.vars(f_lhs(x)), all.vars(f_rhs(x)))
  } else if (is.character(x)) {
    out <- x
  } else if (is_quosures(x) || is.list(x)) {
    out <- unlist(lapply(x, function(q) all.vars(get_expr(q))))
  } else {
    cli::cli_abort(
      "{.arg hierarchy} must be a formula, a character vector, or
      {.fun vars}.",
      call = call
    )
  }

  out <- unique(out[nzchar(out)])
  if (length(out) == 0) {
    cli::cli_abort(
      "{.arg hierarchy} must contain at least one variable.",
      call = call
    )
  }
  out
}

lencode_hier_calc <- function(training, levels, y, wts = NULL, smooth = TRUE) {
  if (!is.numeric(y) && !is.factor(y) && !is.character(y)) {
    cli::cli_abort(
      "Only works nominal or numeric {.arg outcome},
      not {.obj_type_friendly {y}}."
    )
  }

  outcome_type <- if (is.numeric(y)) "numeric" else "nominal"
  if (outcome_type == "nominal") {
    if (is.character(y)) {
      y <- factor(y)
    }
    if (nlevels(y) < 2) {
      cli::cli_abort("A nominal {.arg outcome} must have at least two levels.")
    }
    # The first level is the event of interest, as in the other lencode steps.
    y_num <- as.integer(y == levels(y)[1])
  } else {
    y_num <- as.numeric(y)
  }

  ok <- !is.na(y_num)
  if (!any(ok)) {
    cli::cli_abort("The outcome contains no non-missing values.")
  }
  n_valid <- sum(ok)

  if (!is.null(wts)) {
    wts <- as.numeric(wts)
  }

  lev_values <- lapply(levels, function(l) as.character(training[[l]]))
  names(lev_values) <- levels

  relations <- lencode_hier_relations(lev_values)

  global <- lencode_hier_estimate(
    y_num,
    wts,
    ok,
    outcome_type = outcome_type,
    n_adj = n_valid
  )

  tables <- vector("list", length(levels))
  names(tables) <- levels

  for (j in rev(seq_along(levels))) {
    values <- lev_values[[j]]
    keep <- ok & !is.na(values)
    present <- unique(values[keep])

    parent_values <- if (j == length(levels)) {
      rep(NA_character_, length(present))
    } else {
      unname(relations[[j]][present])
    }

    n <- integer(length(present))
    estimate <- rep(NA_real_, length(present))

    for (i in seq_along(present)) {
      idx <- keep & values == present[i]
      n[i] <- sum(idx)
      raw <- lencode_hier_estimate(
        y_num,
        wts,
        idx,
        outcome_type = outcome_type,
        n_adj = n_valid
      )
      parent <- parent_values[[i]]
      parent_est <- if (
        is.na(parent) ||
          j == length(levels) ||
          !(parent %in% tables[[j + 1]]$level)
      ) {
        global
      } else {
        tables[[j + 1]]$value[tables[[j + 1]]$level == parent][1]
      }
      support <- if (is.null(wts)) n[i] else sum(wts[idx])
      estimate[i] <- lencode_hier_pool(raw, support, parent_est, smooth)
    }

    tables[[j]] <- tibble::tibble(
      level = present,
      value = estimate,
      n = n,
      parent = parent_values
    )
  }

  missing_idx <- ok & is.na(lev_values[[1]])
  n_missing <- sum(missing_idx)
  if (n_missing > 0) {
    raw_missing <- lencode_hier_estimate(
      y_num,
      wts,
      missing_idx,
      outcome_type = outcome_type,
      n_adj = n_valid
    )
    support <- if (is.null(wts)) n_missing else sum(wts[missing_idx])
    missing_value <- lencode_hier_pool(raw_missing, support, global, smooth)
  } else {
    missing_value <- global
  }

  list(
    levels = levels,
    finest = levels[1],
    global = global,
    unseen = global,
    missing = missing_value,
    missing_n = n_missing,
    tables = tables
  )
}

lencode_hier_relations <- function(lev_values) {
  n_levels <- length(lev_values)
  relations <- vector("list", max(n_levels - 1, 0))
  if (n_levels < 2) {
    return(relations)
  }

  for (j in seq_len(n_levels - 1)) {
    child <- lev_values[[j]]
    parent <- lev_values[[j + 1]]
    child_name <- names(lev_values)[j]
    parent_name <- names(lev_values)[j + 1]

    keep <- !is.na(child) & !is.na(parent)
    split_parent <- split(parent[keep], child[keep])
    n_distinct <- vapply(
      split_parent,
      function(x) length(unique(x)),
      integer(1)
    )

    contradictory <- names(n_distinct)[n_distinct > 1]
    if (length(contradictory) > 0) {
      cli::cli_abort(
        c(
          "Contradictory parent relations found for {.var {child_name}}.",
          "i" = "The following values of {.var {child_name}} belong to more
          than one value of {.var {parent_name}}: {.val {contradictory}}."
        )
      )
    }

    isolated <- setdiff(unique(child[!is.na(child)]), unique(child[keep]))
    if (length(isolated) > 0) {
      cli::cli_abort(
        c(
          "Isolated parent relations found for {.var {child_name}}.",
          "i" = "The following values of {.var {child_name}} have no observed
          value of {.var {parent_name}}: {.val {isolated}}."
        )
      )
    }

    relations[[j]] <- vapply(split_parent, function(x) x[1], character(1))
  }

  relations
}

lencode_hier_estimate <- function(y, wts, idx, outcome_type, n_adj) {
  if (!any(idx)) {
    return(NA_real_)
  }

  if (is.null(wts)) {
    p <- mean(y[idx])
  } else {
    p <- stats::weighted.mean(y[idx], wts[idx])
  }

  if (outcome_type == "numeric") {
    return(p)
  }

  log_odds <- log(p / (1 - p))
  adjust_infinities(log_odds, n = n_adj)
}

lencode_hier_pool <- function(raw, support, parent, smooth) {
  if (is.na(raw)) {
    return(parent)
  }
  if (!smooth || is.na(parent)) {
    return(raw)
  }
  (support * raw + 10 * parent) / (support + 10)
}

#' @export
bake.step_lencode_hier <- function(object, new_data, ...) {
  lencode_hier_bake(object, new_data)
}

#' @export
bake.step_lencode_hierarchical <- function(object, new_data, ...) {
  lencode_hier_bake(object, new_data)
}

lencode_hier_bake <- function(object, new_data) {
  if (length(object$mapping) == 0) {
    return(new_data)
  }

  levs <- object$mapping$levels
  check_new_data(levs, object, new_data)

  value <- lencode_hier_map(object$mapping, new_data)

  col_name <- paste0(object$prefix, "_", object$mapping$finest)
  new_col <- tibble::tibble(!!col_name := value)
  new_col <- recipes::check_name(new_col, new_data, object, col_name)

  new_data <- vctrs::vec_cbind(new_data, new_col)
  new_data <- remove_original_cols(new_data, object, levs)

  new_data
}

lencode_hier_map <- function(mapping, new_data) {
  levs <- mapping$levels
  n <- nrow(new_data)
  values <- lapply(levs, function(l) as.character(new_data[[l]]))
  names(values) <- levs

  # Reject rows whose parent values contradict the relations learned in prep.
  lencode_hier_check_relations(mapping, values)

  finest_value <- values[[1]]
  finest <- mapping$tables[[levs[1]]]

  missing <- is.na(finest_value)
  hit <- match(finest_value, finest$level)
  seen <- !is.na(hit) & !missing

  out <- rep(NA_real_, n)
  out[missing] <- mapping$missing
  out[seen] <- finest$value[hit[seen]]

  fallback <- which(!seen & !missing)
  out[fallback] <- mapping$unseen

  if (length(fallback) > 0 && length(levs) > 1) {
    for (j in seq(2, length(levs))) {
      if (length(fallback) == 0) {
        break
      }
      table <- mapping$tables[[levs[j]]]
      parent_hit <- match(values[[j]][fallback], table$level)
      picked <- !is.na(parent_hit)
      out[fallback[picked]] <- table$value[parent_hit[picked]]
      fallback <- fallback[!picked]
    }
  }

  out
}

lencode_hier_check_relations <- function(mapping, values) {
  levs <- mapping$levels
  if (length(levs) < 2) {
    return(invisible(NULL))
  }

  for (j in seq(2, length(levs))) {
    child_table <- mapping$tables[[levs[j - 1]]]
    child_value <- values[[j - 1]]
    observed <- values[[j]]

    expected <- child_table$parent[match(child_value, child_table$level)]
    bad <- !is.na(expected) & !is.na(observed) & expected != observed
    if (any(bad)) {
      i <- which(bad)[1]
      cli::cli_abort(
        c(
          "Contradictory parent relation in row {i}.",
          "i" = "{.val {values[[j - 1]][i]}} is a value of {.var {levs[j - 1]}}
          that belongs to {.val {expected[i]}} in {.var {levs[j]}}, not
          {.val {observed[i]}}."
        )
      )
    }
  }

  invisible(NULL)
}

#' @export
print.step_lencode_hier <- function(x, ...) {
  lencode_hier_print(x, ...)
}

#' @export
print.step_lencode_hierarchical <- function(x, ...) {
  lencode_hier_print(x, ...)
}

lencode_hier_print <- function(x, width = max(20, options()$width - 31), ...) {
  title <- "Hierarchical likelihood encoding for "
  if (is_trained(x)) {
    print_step(
      x$hierarchy,
      x$terms,
      TRUE,
      title,
      width,
      case_weights = x$case_weights
    )
  } else {
    untrained <- if (is.null(x$hierarchy)) {
      x$terms
    } else {
      rlang::quos(!!!rlang::syms(hier_names(x$hierarchy)))
    }
    print_step(
      NULL,
      untrained,
      FALSE,
      title,
      width,
      case_weights = x$case_weights
    )
  }
  invisible(x)
}

#' @rdname step_lencode_hier
#' @usage NULL
#' @export
tidy.step_lencode_hier <- function(x, ...) {
  lencode_hier_tidy(x)
}

#' @export
tidy.step_lencode_hierarchical <- function(x, ...) {
  lencode_hier_tidy(x)
}

lencode_hier_tidy <- function(x) {
  if (is_trained(x)) {
    if (length(x$mapping) == 0) {
      res <- tibble(
        terms = character(),
        level = character(),
        value = double(),
        n = integer(),
        parent = character()
      )
    } else {
      res <- lencode_hier_tidy_mapping(x$mapping)
    }
  } else {
    term_names <- if (is.null(x$hierarchy)) {
      sel2char(x$terms)
    } else {
      hier_names(x$hierarchy)
    }
    res <- tibble(
      terms = term_names,
      level = rep(na_chr, length(term_names)),
      value = rep(na_dbl, length(term_names)),
      n = rep(na_int, length(term_names)),
      parent = rep(na_chr, length(term_names))
    )
  }
  res$id <- x$id
  res
}

lencode_hier_tidy_mapping <- function(mapping) {
  tables <- mapping$tables
  res <- lapply(seq_along(tables), function(i) {
    table <- tables[[i]]
    tibble(
      terms = names(tables)[i],
      level = table$level,
      value = table$value,
      n = as.integer(table$n),
      parent = table$parent
    )
  })
  res <- bind_rows(res)

  special <- tibble(
    terms = rep(mapping$finest, 2),
    level = c("..new", "..missing"),
    value = c(mapping$unseen, mapping$missing),
    n = c(0L, as.integer(mapping$missing_n)),
    parent = c(NA_character_, NA_character_)
  )

  bind_rows(res, special)
}

#' @export
required_pkgs.step_lencode_hier <- function(x, ...) {
  c("embed")
}

#' @export
required_pkgs.step_lencode_hierarchical <- function(x, ...) {
  c("embed")
}


