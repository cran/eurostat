#' @title Cuts the Values Column into Classes and Polishes the Labels
#' @description Categorises a numeric vector into automatic or manually defined
#' categories and polishes the labels ready for used in mapping with `ggplot2`.
#' @param x A numeric vector, eg. `values` variable in data returned by
#'   [get_eurostat()].
#' @param n A numeric. number of classes/categories
#' @param manual Logical. If manual breaks are being used
#' @param manual_breaks Numeric vector with manual threshold values
#' @param decimals Number of decimals to include with labels
#' @param nodata_label String. Text label for NA category.
#' @inheritParams classInt::classIntervals
#' @author Markus Kainu <markuskainu@@gmail.com>
#' @return a factor.
#' @examplesIf check_access_to_data()
#' \donttest{
#' # lp <- get_eurostat("nama_aux_lp")
#' lp <- get_eurostat("nama_10_lp_ulc")
#' lp$class <- cut_to_classes(lp$values, n = 5, style = "equal", decimals = 1)
#' }
#'
#' @importFrom classInt classIntervals
#' @importFrom stringr str_replace_all
#' @seealso [classInt::classIntervals()]
#' @family helpers
#' @export
cut_to_classes <- function(x, n = 5, style = "equal", manual = FALSE,
                           manual_breaks = NULL, decimals = 0,
                           nodata_label = "No data") {
  # manual_breaks_orig <- manual_breaks
  if (!is.null(manual_breaks) &&
        (length(unique(manual_breaks)) == length(manual_breaks))) {
    warning(paste("manual_breaks in cut_to_classes are not unique.",
                  "Using unique breaks only."))
    manual_breaks <- unique(manual_breaks)
  }

  if (manual) {
    levs <- as.data.frame(
      levels(
        cut(
          x,
          breaks = manual_breaks,
          include.lowest = TRUE,
          dig.lab = 5
        )
      )
    )
  } else {
    brs <- data.frame(classInt::classIntervals(x, n = n, style = style)[2])[, 1]

    # Ensure that the breaks are unique
    brs <- unique(brs)

    cutting <- cut(x, breaks = brs, include.lowest = TRUE, dig.lab = 5)

    levs <- levels(cutting)

    levs <- as.data.frame(levs)
  }

  names(levs) <- "orig"
  levs$mod <- stringr::str_replace_all(levs$orig, "\\[", "")
  levs$mod <- stringr::str_replace_all(levs$mod, "\\]", "")
  levs$mod <- stringr::str_replace_all(levs$mod, "\\(", "")
  levs$lower <- gsub(",.*$", "", levs$mod)
  levs$upper <- gsub(".*,", "", levs$mod)

  levs$lower <- factor(levs$lower)
  levs$lower <- round(as.numeric(levels(levs$lower))[levs$lower], decimals)
  levs$lower <- prettyNum(levs$lower, big.mark = " ")

  levs$upper <- factor(levs$upper)
  levs$upper <- round(as.numeric(levels(levs$upper))[levs$upper], decimals)
  levs$upper <- prettyNum(levs$upper, big.mark = " ")

  levs$labs <- paste(levs$lower, levs$upper, sep = " ~< ")

  labs <- as.character(c(levs$labs))

  if (manual) {
    y <- cut(x,
      breaks = manual_breaks,
      include.lowest = TRUE,
      dig.lab = 5, labels = labs
    )
    rm(manual_breaks)
  } else {
    brs2 <- data.frame(classIntervals(x, n = n, style = style)[2])[, 1]

    # Ensure the breaks are unique
    brs2 <- unique(brs2)

    y <- cut(x,
      breaks = brs2,
      include.lowest = TRUE,
      dig.lab = 5, labels = labs
    )
  }
  y <- as.character(y)

  y[is.na(y)] <- nodata_label

  y <- factor(y, levels = c(nodata_label, labs[1:n]))

  y
}
