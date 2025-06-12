# Ensure required packages are installed and loaded
if (!require("microbenchmark")) install.packages("microbenchmark")
if (!require("data.table")) install.packages("data.table")
if (!require("testthat")) install.packages("testthat")
library(microbenchmark)
library(data.table)
library(testthat)

# Original function, restored to its initial, untouched version.
# NOTE: This version is not only slow but can be fragile with very large
# inputs due to its use of nested `apply`. It is kept for comparison.
.compress_mat_original <- function(mat, line_grouping, collapse_method = c("nl", "unique")) {
  list_compacted_mat <- lapply(unique(line_grouping), function(lg) {
    apply(mat, 2, function(mat_cols) {
      col_vec <- mat_cols[which(line_grouping == lg)]
      if (collapse_method[1] == "nl") {
        paste0(col_vec, collapse = "\n")
      } else {
        val <- unique(col_vec)
        val <- val[nzchar(val)]
        if (length(val) > 1) {
          stop("Problem in linegroupings! Some do not have the same values.") # nocov
        } else if (length(val) < 1) {
          val <- "" # Case in which it is only ""
        }
        val[[1]]
      }
    })
  })
  do.call("rbind", list_compacted_mat)
}

# --- Optimized Versions ---

# 1. Optimized Base R version
# This version now ensures its output has the exact same row order as the original function.
.compress_mat_base_R <- function(mat, line_grouping, collapse_method = c("nl", "unique")) {
  df <- as.data.frame(mat, stringsAsFactors = FALSE)

  # The original function processes groups in the order they appear in `unique(line_grouping)`.
  # We create a factor with levels set to that specific order to force `split` to maintain it.
  factor_grouping <- factor(line_grouping, levels = unique(line_grouping))
  list_of_dfs <- split(df, factor_grouping)

  if (collapse_method[1] == "nl") {
    result_list <- lapply(list_of_dfs, function(sub_df) {
      sapply(sub_df, paste, collapse = "\n")
    })
  } else { # "unique" method
    result_list <- lapply(list_of_dfs, function(sub_df) {
      sapply(sub_df, function(col) {
        val <- unique(col[nzchar(col)])
        if (length(val) > 1) {
          stop("Problem in linegroupings! Some do not have the same values.")
        } else if (length(val) == 0) {
          ""
        } else {
          val
        }
      })
    })
  }
  do.call("rbind", result_list)
}


# 2. Optimized data.table version
# This version now ensures its output has the exact same row order as the original function.
.compress_mat_datatable <- function(mat, line_grouping, collapse_method = c("nl", "unique")) {
  dt <- as.data.table(mat)
  dt[, `:=` (group_col = line_grouping)]

  # The `by` operation sorts the groups by default.
  # We will reorder the result later to match the original function's output order.
  if (collapse_method[1] == "nl") {
    result_dt <- dt[, lapply(.SD, paste, collapse = "\n"), by = group_col]
  } else { # "unique" method
    unique_func <- function(col) {
      val <- unique(col[nzchar(col)])
      if (length(val) > 1) stop("Problem in linegroupings! Some do not have the same values.")
      if (length(val) == 0) "" else val
    }
    result_dt <- dt[, lapply(.SD, unique_func), by = group_col]
  }

  # Reorder the results to match the original function's group order.
  original_order <- unique(line_grouping)
  setkeyv(result_dt, "group_col") # Set a key for fast matching
  final_dt <- result_dt[J(original_order)] # Reorder using a join-like operation

  final_dt[, group_col := NULL]
  as.matrix(final_dt)
}


# --- Data Generation ---

# Using a slightly smaller dataset to prevent the fragile original function from erroring.
set.seed(42) # for reproducibility
n_rows <- 1000
n_cols <- 10
n_groups <- 100

line_grouping <- sample(1:n_groups, n_rows, replace = TRUE)

strmat <- matrix(
  paste0("s_", sample(1:(n_rows / 5), n_rows * n_cols, replace = TRUE)),
  nrow = n_rows
)

group_vals <- matrix(
  paste0("f_", 1:(n_groups * n_cols)),
  nrow = n_groups,
  ncol = n_cols
)
frmat <- group_vals[line_grouping, ]
frmat[sample(length(frmat), size = n_rows * 0.1)] <- ""


# --- Verification ---
# We verify that the outputs of the optimized functions are identical to the original.
# We set the attributes to NULL to ensure we are only comparing the matrix data values.
cat("--- Verifying 'nl' method results are identical ---\n")
res_orig_nl <- .compress_mat_original(strmat, line_grouping, "nl")
res_base_nl <- .compress_mat_base_R(strmat, line_grouping, "nl")
res_dt_nl   <- .compress_mat_datatable(strmat, line_grouping, "nl")

test_that("Optimized 'nl' methods match original", {
  # Set attributes to NULL as they are not needed and can differ.
  attributes(res_orig_nl) <- NULL
  attributes(res_base_nl) <- NULL
  attributes(res_dt_nl) <- NULL
  expect_equal(res_base_nl, res_orig_nl)
  expect_equal(res_dt_nl, res_orig_nl)
})
cat("Verification successful.\n\n")


cat("--- Verifying 'unique' method results are identical ---\n")
res_orig_unique <- .compress_mat_original(frmat, line_grouping, "unique")
res_base_unique <- .compress_mat_base_R(frmat, line_grouping, "unique")
res_dt_unique   <- .compress_mat_datatable(frmat, line_grouping, "unique")

test_that("Optimized 'unique' methods match original", {
  attributes(res_orig_unique) <- NULL
  attributes(res_base_unique) <- NULL
  attributes(res_dt_unique) <- NULL
  expect_equal(res_base_unique, res_orig_unique)
  expect_equal(res_dt_unique, res_orig_unique)
})
cat("Verification successful.\n\n")


# --- Benchmark ---

cat("--- Benchmarking 'nl' (newline) method ---\n")
bm_nl <- microbenchmark(
  original = .compress_mat_original(strmat, line_grouping, "nl"),
  base_R   = .compress_mat_base_R(strmat, line_grouping, "nl"),
  datatable= .compress_mat_datatable(strmat, line_grouping, "nl"),
  times = 10L
)
print(bm_nl)

cat("\n\n--- Benchmarking 'unique' method ---\n")
bm_unique <- microbenchmark(
  original = .compress_mat_original(frmat, line_grouping, "unique"),
  base_R   = .compress_mat_base_R(frmat, line_grouping, "unique"),
  datatable= .compress_mat_datatable(frmat, line_grouping, "unique"),
  times = 10L
)
print(bm_unique)
