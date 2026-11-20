# constant predictors are not binned

    Code
      splits <- embed:::monotonic_binning(rep(1, 8), "x", sim8$class, event_level = "first",
      num_breaks = 8, min_weight = 0, mono = "auto")
    Condition
      Warning:
      `step_discretize_monotonic()` failed to find any meaningful splits for predictor "x", which will not be binned.

# step function for classification

    Code
      mono_rec <- prep(step_discretize_monotonic(recipe(class ~ ., data = sim8), x,
      outcome = "class", num_breaks = 8, min_weight = 0))

# case weights are used

    Code
      rec_w <- prep(step_discretize_monotonic(recipe(class ~ ., data = mutate(sim_w,
        w = importance_weights(wts))), x, outcome = "class", num_breaks = 100,
      min_weight = 0, mono = "increasing"))

---

    Code
      rec_w
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    1
      case_weights: 1
      
      -- Training information 
      Training data contained 35 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using monotonic binning: <none> | Trained, weighted

# bad args

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = sim8), x, outcome = "class",
      num_breaks = 1))
    Condition
      Error in `step_discretize_monotonic()`:
      Caused by error in `prep()`:
      ! `num_breaks` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = sim8), x, outcome = "class",
      num_breaks = 2.5))
    Condition
      Error in `step_discretize_monotonic()`:
      Caused by error in `prep()`:
      ! `num_breaks` must be a whole number, not the number 2.5.

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = sim8), x, outcome = "class",
      min_weight = -1))
    Condition
      Error in `step_discretize_monotonic()`:
      Caused by error in `prep()`:
      ! `min_weight` must be a number larger than or equal to 0, not the number -1.

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = sim8), x, outcome = "class",
      mono = "sideways"))
    Condition
      Error in `step_discretize_monotonic()`:
      ! `mono` must be one of "auto", "increasing", or "decreasing", not "sideways".

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = sim8), x, outcome = "class",
      event_level = "third"))
    Condition
      Error in `step_discretize_monotonic()`:
      ! `event_level` must be one of "first" or "second", not "third".

---

    Code
      step_discretize_monotonic(recipe(class ~ ., data = sim8), x)
    Condition
      Error in `step_discretize_monotonic()`:
      ! The `outcome` argument should select at least one column.

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = mutate(sim8, class = 1:
      8)), x, outcome = "class"))
    Condition
      Error in `step_discretize_monotonic()`:
      Caused by error in `prep()`:
      ! `outcome` must select a factor or character column, not an integer vector.

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = mutate(sim8, class = factor(
        rep(letters[1:3], length.out = 8)))), x, outcome = "class"))
    Condition
      Error in `step_discretize_monotonic()`:
      Caused by error in `prep()`:
      ! `outcome` must select a column with exactly 2 classes, not 3.

---

    Code
      prep(step_discretize_monotonic(recipe(class ~ ., data = mutate(sim8, ch = letters[
        1:8])), x, ch, outcome = "class"))
    Condition
      Error in `step_discretize_monotonic()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `ch`

# bake method errors when needed non-standard role columns are missing

    Code
      rec_trained <- prep(rec, training = sim8, verbose = FALSE)

---

    Code
      bake(rec_trained, new_data = sim8[, -1])
    Condition
      Error in `step_discretize_monotonic()`:
      ! The following required column is missing from `new_data`: x.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Discretizing variables using monotonic binning: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using monotonic binning: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Operations 
      * Discretizing variables using monotonic binning: x

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Training information 
      Training data contained 8 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using monotonic binning: x | Trained

