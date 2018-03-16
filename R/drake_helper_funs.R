#' Evaluate multiple wildcard values together
#' 
#' Replace multiple wildcards with multiple values using a rules_list, a named list of elements to map wildcards (list names) to their values (list element values).
#' What makes this different from \code{evaluate_plan()} is that we use multiple vectors to replace wildcards and want to use the 1st element of all vectors, and then the 2nd value, etc... 
#'
#' @param plan A \code{drake_plan()} output. Or any data.frame that is a valid drake plan.
#' @param rules_list A named list of rules. Element names are the wildcards to replace and list element values are expected to be values to replace wildcards with.
#' @param names_list List of names to append to the target names. 
#'
#' @details The rules_list argument is a list of vectors, where each vector is preferably of the same length. The first list elements are used together, and then the 2nd, etc...
#' However, if only some of the vectors have a length > 1, and other elements are of length 1, then the values with length 1 are repeated so they match the longer vectors.
#' All vectors with length > 1 should be the same length! 
#'
#' @return
#' @export
#'
#' @examples
expandMultiple <- function(plan, rules_list, names_list = NULL) {
  
  rules_length <- unlist(lapply(rules_list, length))
  if (all(rules_length == 1)) {
    message("Only single rule given")
    return(plan %>% evaluate_plan(rules = rules_list))
    
  } else if (any(rules_length) == 1 & length(unique(rules_length)) == 2) {
    message("Some rules only have a single value and will be repeated.")
    max_length <- max(rules_length)
    for (i in which(rules_length==1)) rules_list[[i]] <- rep(rules_list[[i]], max_length)
    
    # If more than 2 length, return error
  } else if (length(unique(rules_length)) > 2) {
    warning("Element of rules_list not of equal length! No expansion done")
    return(plan)
  }

    new_plan <- lapply(1:max(rules_length), function(i) {
    plan.tmp <- evaluate_plan(plan, rules = lapply(rules_list, `[`, i), expand = FALSE)
    if (!is.null(names_list[[i]])) plan.tmp$target <- paste0(plan.tmp$target, "_", names_list[[i]])
    plan.tmp
  })
  
  return(do.call(rbind, new_plan))
}

# Function to wrap a string in escaped single quotes. Strings are automatically unquoted when using drake::evaluate_plan() to replace wildcards. 
# But, in the case of file dependencies we need to wrap the filename in single quotes. 
# (Double quotes are for literal strings, not dependencies.)
WrapQuote <- function(string, quote = "\'") paste0(quote, string, quote)


# Function to wrap a string in escaped double quotes. Strings are automatically unquoted when using drake::evaluate_plan() to replace wildcards. 
# Double quotes are used for literal string values.
WrapQuoteDbl <- function(string) WrapQuote(string, quote = "\"")