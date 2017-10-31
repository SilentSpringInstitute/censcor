#' @importFrom stringr str_split
parse_formula <- function(f, data) {
  # for example:
  # x|cens(cens_x) ~ y|cens(cens_y)
  # or:
  # x|cens(cens_x, x_upper) ~ y|cens(cens_y, y_upper)
  
  # Every formula must have a left and right hand side
  sides <- str_split(f, "~")
  if(length(sides) != 3) {
    stop("incorrect formula: formula must have two sides, e.g. y ~ x")
  }
  
  lhs = parse_formula_side(sides[[2]], data)
  rhs = parse_formula_side(sides[[3]], data)
  
  list(
    x = lhs$x,
    cens_x = lhs$cens_x,
    cens_x_upper = lhs$cens_x_upper,
    y = rhs$x,
    cens_y = rhs$cens_x,
    cens_y_upper = rhs$cens_x_upper
  )
}

#' @importFrom stringr str_detect
parse_formula_side <- function(side, data) {
  x <- terms(as.formula(paste0("~", side)))
  
  term <- attr(x, "term.labels")[1]
  
  # Look for pipe
  parts <- str_split(term, "\\|")[[1]]
  
  ret <- list()
  
  ret$x <- eval(parse(text = parts[1]), data)
  if(length(parts) == 2) {
    if(str_detect(parts[2], "cens")) {
      cens_list = eval(parse(text = parts[2]), list(data = data, cens = cens))
      ret$cens_x <- cens_list$cens
      ret$cens_x_upper <- cens_list$upper
    }
  }
  
  ret
}

cens <- function(x, upper = NULL) {
  x <- as.list(match.call())
  list(
    cens = prep_cens(eval(x$x, parent.frame(4)$data)),
    upper = eval(x$upper, parent.frame(4)$data)
  )
}

prep_cens <- function(x) {
  if(is.factor(x)) {
    x <- as.character(x)
  }
  
  if(is.character(x)) {
    if(sum(x %in% c("left", "none", "right", "interval")) != length(x)) {
      stop("Censoring indicator variable must be \"left\", \"none\", \"right\", or \"interval\"")
    }
    
    x <- ifelse(x == "left", -1,
                ifelse(x == "none", 0,
                ifelse(x == "right", 1,
                ifelse(x == "interval", 2))))
  }
  else if(is.numeric(x)) {
    if(sum(x %in% -1:2) != length(x)) {
      stop("Censoring indicator must be -1, 0, 1, or 2")
    }
  }
  
  x
}
