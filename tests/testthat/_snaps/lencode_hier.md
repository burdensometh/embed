# contradictory parent relations are rejected

    Code
      prep(hier_recipe(dat, product, category, department))
    Condition
      Error in `step_lencode_hier()`:
      Caused by error in `lencode_hier_relations()`:
      ! Contradictory parent relations found for `product`.
      i The following values of `product` belong to more than one value of `category`: "a", "b", "c", and "d".

# isolated children are rejected

    Code
      prep(hier_recipe(dat, product, category, department))
    Condition
      Error in `step_lencode_hier()`:
      Caused by error in `lencode_hier_relations()`:
      ! Isolated parent relations found for `product`.
      i The following values of `product` have no observed value of `category`: "d".

# contradictory parent relations are rejected during bake

    Code
      bake(rec, new_data = new_dat)
    Condition
      Error in `lencode_hier_check_relations()`:
      ! Contradictory parent relation in row 1.
      i "a" is a value of `product` that belongs to "x" in `category`, not "y".

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_lencode_hier()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = dat[, -4])
    Condition
      Error in `step_lencode_hier()`:
      ! The following required column is missing from `new_data`: department.

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Operations 
      * Hierarchical likelihood encoding for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 8 data points and no incomplete rows.
      
      -- Operations 
      * Hierarchical likelihood encoding for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Operations 
      * Hierarchical likelihood encoding for: product, category, department

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 8 data points and no incomplete rows.
      
      -- Operations 
      * Hierarchical likelihood encoding for: product category, ... | Trained

