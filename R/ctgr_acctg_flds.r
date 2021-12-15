#' Auto categorize format types to pass into 'fmt_acctg_tbl'
#'
#' @param data_tbl
#' @param suppress_fields tells the formatter to suppress formatting for a specified field name
#'
#' @return
#' @export
#'
#' @examples ctgr_acctg_flds(dplyr::rename(mtcars, cyl_cnt = cyl))
ctgr_acctg_flds <- function(data_tbl, ignore_fields = NULL){

  data_tbl <- dplyr::select(data_tbl, -dplyr::all_of(ignore_fields))

  nums <- data_tbl %>% purrr::map_lgl(is.numeric)
  dollar_nms <- data_tbl %>%
    names() %>%
    stringr::str_detect('(?i)bal|inc|exp')
  pcnt_nms <- data_tbl %>%
    names() %>%
    stringr::str_detect('(?i)percent|pcnt|proportion')
  rate_nms <- data_tbl %>%
    names() %>%
    stringr::str_detect('(?i)freq|rate')
  count_nms <- data_tbl %>%
    names() %>%
    stringr::str_detect('(?i)num|count|cnt')

  numbers <- names(nums)
  percents <- names(nums)[nums & pcnt_nms]
  rates <- names(nums)[nums & rate_nms]  %>% dplyr::setdiff(., percents)
  dollars <- names(nums)[nums & dollar_nms] %>% dplyr::setdiff(., union(rates, percents))
  counts <- names(nums)[nums & count_nms] %>% dplyr::setdiff(., union(dollars, union(rates, percents)))

  list(numbers=numbers,percents=percents,rates=rates,dollars=dollars,counts=counts)

}

