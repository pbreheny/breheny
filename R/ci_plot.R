#' Create forest plot of confidence intervals
#'
#' "Forest plot"-style plotting of confidence intervals, typically from a
#' regression model. Basic input is a matrix with columns of
#' estimate/lower/upper, along with an optional 4th column for the p-value. Also
#' works with a variety of models (lm/glm/coxph/etc).
#'
#' TO-DO:
#' * interactions
#'
#' @param obj  The object to be plotted. This can be either:
#'   * A data frame with variables in the following order:
#'     1. (Optional) Label - if omitted row names will be used instead
#'     2. Estimate
#'     3. Lower CI limit
#'     4. Upper CI limit
#'     5. (Optional) p-value
#'
#'   * A model object (e.g., from `lm()`, `glm()`, etc.), which will be automatically
#'   converted to a data frame in this format.
#'
#'   See examples for usage of both.
#' @param ...    Not used; for S3 compatibility
#'
#' @seealso [ci_int()]
#'
#' @examples
#' # Supplying a data frame
#' b <- data.frame(1:9, 0:8, 2:10)
#' rownames(b) <- LETTERS[1:9]
#' ci_plot(b)
#' rownames(b)[3] <- "This one is long"
#' ci_plot(b)
#'
#' @export

ci_plot <- function(obj, ...) UseMethod("ci_plot")

#' @rdname ci_plot
#'
#' @param xlab    Horizontal axis label (default: 'Estimate')
#' @param null    Draw a line representing no effect at this value (default: 0)
#' @param exp     Should the axis labels be exponentiated because the estimates
#'   are on a log scale? (default: FALSE)
#' @param brk     If `exp = TRUE`, you can supply "pretty" breaks (numeric vector)
#' @param return  One of the following:
#'   * `gg`: Return the complete plot as a patchwork / gg object (default)
#'   * `df`: Return the formatted data frame (don't construct a plot at all)
#'   * `list`: Return the plot components as a list (useful if you want to
#'     further modify one of the components; see examples)
#'
#' @export

ci_plot.data.frame <- function(obj,
                               xlab = "Estimate",
                               null = 0,
                               exp = FALSE,
                               brk,
                               return = c("gg", "df", "list"),
                               ...) {
  return <- match.arg(return)
  if (return == "df") {
    return(obj)
  }

  # Set up CI data frame
  if (is.character(obj[[1]])) {
    lab <- obj[[1]]
    obj[[1]] <- NULL
  } else {
    lab <- rownames(obj)
  }
  lab <- factor(lab, rev(lab))
  if (ncol(obj) < 3 || ncol(obj) > 4) {
    stop("The object you are passing to ci_plot() is not formatted correctly. See ?ci_plot.",
      call. = FALSE
    )
  }

  colnames(obj)[1:3] <- c("est", "lwr", "upr")
  df <- data.frame(
    name = lab,
    obj[, 1:3],
    even = rep_len(c("no", "yes"), nrow(obj)),
    start = seq(0.5, nrow(obj), 1),
    end = seq(0.5, nrow(obj), 1) + 1
  )

  # Plot
  g <- ggplot2::ggplot(df, ggplot2::aes(.data$est, .data$name, fill = .data$even)) +
    ggplot2::geom_rect(
      ggplot2::aes(ymin = .data$start, ymax = .data$end),
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.2
    ) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::geom_segment(ggplot2::aes(
      x = .data$lwr,
      xend = .data$upr,
      y = .data$name,
      yend = .data$name
    )) +
    ggplot2::ylab("") +
    ggplot2::scale_fill_manual(values = c("white", "gray75")) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0))

  # Add null line
  if (!is.null(null)) {
    g <- g + ggplot2::geom_vline(xintercept = null, lty = 2, col = "gray70")
  }

  # Exponentiate labels
  if (exp) {
    if (missing(brk)) {
      g <- g + ggplot2::scale_x_continuous(
        labels = scales::trans_format("exp", scales::label_number())
      )
    } else {
      g <- g + ggplot2::scale_x_continuous(
        breaks = log(brk),
        labels = scales::trans_format("exp", scales::label_number())
      )
    }
  }

  # Create p-value sidebar
  if (ncol(obj) == 4) {
    if (is.numeric(obj[[4]])) {
      df$p <- format_p(obj[[4]])
    } else {
      df$p <- obj[[4]]
    }
    p <- ggplot2::ggplot(df, ggplot2::aes(0, .data$name, label = .data$p)) +
      ggplot2::geom_text(hjust = "left", size = 3) +
      ggplot2::theme_void() +
      ggplot2::coord_cartesian(clip = "off", xlim = c(0, 0.5))
    if (return == "list") {
      return(list(ci = g, p = p))
    }
    g <- g + ggplot2::xlab(xlab) + p + patchwork::plot_layout(widths = c(10, 1))
  }
  g
}

#' @rdname ci_plot
#' @param tau        A named vector of effect sizes; CIs will be shown for
#'   tau*beta. Any coefficients not included are given tau = 1. To exclude a
#'   variable completely, use tau = 0.
#' @param intercept  Include a CI for the intercept? (default: FALSE)
#' @param p          Include p-values? (default: TRUE)
#'
#' @examples
#' # Supplying a fitted model object
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, airquality)
#' ci_plot(fit)
#' ci_plot(fit, tau = c(Solar.R = 100, Wind = 0))
#' ci_plot(fit, p = FALSE)
#' @export

ci_plot.lm <- function(obj, tau, intercept = FALSE, p = TRUE, ...) {
  j <- if (intercept) 1:length(coef(obj)) else 2:length(coef(obj))
  b <- data.frame(
    coef(obj)[j],
    confint(obj, j)
  )
  if (p) {
    b <- cbind(b, summary(obj)$coef[j, 4])
  }
  if (!missing(tau)) b <- ci_contrast(b, tau)
  ci_plot(b, ...)
}

#' @rdname ci_plot
#' @examples
#' fit <- glm(case ~ spontaneous + induced, data = infert, family = binomial())
#' ci_plot(fit)
#' ci_plot(fit, exp = TRUE, xlab = "Odds ratio")
#' @export
ci_plot.glm <- function(obj, ...) ci_plot.lm(obj, ...)

#' @rdname ci_plot
#' @examples
#' library(lme4)
#' fit <- lmer(Reaction ~ Days + I(Days^2) + (1 | Subject), sleepstudy)
#' ci_plot(fit)
#' @export
ci_plot.merMod <- function(obj, tau, intercept = FALSE, ...) {
  fix <- lme4::fixef(obj)
  j <- names(fix)
  if (!intercept) j <- setdiff(j, "(Intercept)")
  b <- data.frame(fix[j], confint(obj, j, quiet = TRUE))
  if (!missing(tau)) b <- ci_contrast(b, tau)
  ci_plot(b, ...)
}

#' @rdname ci_plot
#' @examples
#' library(survival)
#' fit <- coxph(Surv(time, status) ~ trt + karno, veteran)
#' ci_plot(fit)
#' @export
ci_plot.coxph <- function(obj, tau, p = TRUE, ...) {
  j <- 1:length(coef(obj))
  b <- data.frame(
    coef(obj)[j],
    confint(obj, j)
  )
  if (p) {
    b <- cbind(b, summary(obj)$coef[j, 5])
  }
  if (!missing(tau)) b <- ci_contrast(b, tau)
  ci_plot(b, ...)
}

#' @rdname ci_plot
#' @export
ci_plot.matrix <- function(obj, ...) {
  as.data.frame(obj) |> ci_plot.data.frame(...)
}

#' @rdname ci_plot
#' @examples
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, airquality)
#' gtsummary::tbl_regression(fit) |> ci_plot()
#' @export
ci_plot.tbl_regression <- function(obj, ...) {
  tbl <- as.data.frame(obj)
  df <- data.frame(
    tbl[[1]],
    as.numeric(tbl[[2]]),
    stringr::str_split_i(tbl[[3]], ",", 1) |> as.numeric(),
    stringr::str_split_i(tbl[[3]], ",", 2) |> as.numeric(),
    tbl[[4]]
  )
  ci_plot(df[!is.na(df[[2]]), ], ...)
}

ci_contrast <- function(b, tau) {
  if (is.null(names(tau)) || (!all(names(tau) %in% rownames(b)))) {
    stop(
      "tau must be a named vector with names that correspond to coefficients in the model",
      call. = FALSE
    )
  }
  ord <- match(names(tau), rownames(b))
  b[ord, 1:3] <- b[ord, 1:3] * tau
  for (i in names(which(tau == 0))) {
    b <- b[-grep(i, rownames(b)), , drop = FALSE]
  }
  b
}
