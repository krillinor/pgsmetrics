gv <- c(
    ".",
    "..data_cols",
    "..cols_to_use",
    "..covars",
    "..data_cols",
    "..m_covars",
    "..pgs",
    "boot_i",
    "ci_lower",
    "ci_upper",
    "cov",
    "diff_full",
    "full",
    "full_pgs1",
    "full_pgs2",
    "metric",
    "na.omit",
    "observed",
    "partial",
    "pgs",
    "pgs1",
    "pgs2",
    "pgs1_pgs2",
    "pp_l",
    "pp_u",
    "rank_full",
    "rank_partial",
    "type",
    "value"
)
if (getRversion() >= "2.15.1") utils::globalVariables(gv)

pbapply::pboptions(char = "*", txt.width = 20)
