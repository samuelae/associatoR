#' Create target participant cross tables
#'
#' @param associations
#' @param participant_vars
#' @param target_var
#' @param normalize
#'
#' @return
#' @export
#'
#' @examples
ar_cross_targets <- function(associations,
                             participant_vars,
                             target_var,
                             normalize = FALSE) {


  # from ar_compare_targets vvvv ------

  # checks
  check_object(associations)
  check_targets(associations)
  if(chk::vld_function(fun)) {
    chk::chk_function(fun)
  } else if (chk::vld_character(fun)) {
    chk::chk_subset(fun, c("count"))
  } else {
    stop(chk::message_chk("the argument `fun` must be either of type character and match \"count\", or be a function"))
  }

  # get vars and check
  p_vars = dplyr::enquo(participant_vars)
  t_var = dplyr::enquo(target_var)

  # get groups
  data = associations$responses %>%
    dplyr::left_join(associations$participants %>% dplyr::select(id, !!p_vars), by = "id") %>%
    dplyr::filter(response %in% associations$targets$target) %>%
    dplyr::left_join(associations$targets, by = c("response" = "target")) %>%
    dplyr::mutate(target = response)

  if(!is.function(fun)) {
    # fun is predefined
    if(fun[1] == "count") {
      # do counts
      out = data %>%
        dplyr::group_by(dplyr::across(c(!!p_vars, !!t_var))) %>%
        dplyr::summarize(n = length(!!t_var)) %>%
        dplyr::ungroup() %>%
        tidyr::pivot_wider(names_from = !!t_var,
                           values_from = n) %>%
        dplyr::mutate_all(tidyr::replace_na, 0)
    }
  } else {
    # do custom fun, potentially with ... arguments
    out = data %>%
      dplyr::group_by(dplyr::across(c(!!p_vars))) %>%
      dplyr::summarize(!!rlang::ensym(target_var) := fun(!!t_var, ...)) %>%
      dplyr::ungroup()
  }

  # out
  out

}


