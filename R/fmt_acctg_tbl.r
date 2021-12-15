#' Create an accounting DT format for summary tables
#'
#' @param data_tbl table of summary accounting values
#' @param formats_nms output from function categorize_fields() in format list(percents = c(), dollars = c(), counts = c())
#' @param tbl_caption caption to display above table
#' @param has_totals whether the acctg table has a totals column
#'
#' @return an html table with formatting determined by format_nms
#' @export
#'
#' @examples fmt_acctg_tbl(tibble::tibble(balance = -3:5, percent = seq(.1, .5, by = .05)))
fmt_acctg_tbl <- function(data_tbl,
                          formats_nms = ctgr_acctg_flds(data_tbl),
                          tbl_caption = paste0('Data as of ', Sys.Date()),
                          has_totals = T){


  # format DT ---------------------------------------------------------------
  get_odd_rownums <- function(num = nrow(data_tbl)){(1:num)[purrr::rep_along(1:num, c(T,F))]}
  get_even_rownums <- function(num = nrow(data_tbl)){(1:num)[purrr::rep_along(1:num, c(F,T))]}

  max_row <- nrow(data_tbl)
  odd_rownums <- get_odd_rownums(max_row)
  even_rownums <- get_even_rownums(max_row)

  out <- data_tbl %>%
    DT::datatable(.,
                  selection = 'none',
                  rownames = F,
                  escape = F,
                  caption = tbl_caption,
                  extensions = c('Buttons'),
                  options = list(
                    ordering=F,
                    searchHighlight = TRUE,
                    dom = 'tB',
                    pageLength = nrow(data_tbl),
                    buttons = list('print',list(extend = 'collection',
                                                buttons = c('pdf', 'csv','excel'),
                                                text = 'Download')),
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#404040', 'color': '#fff', 'font-size': '95%',
                        'text-align':'left', 'background-image':'linear-gradient(180deg, transparent 95%, black 95%'}); ",
                      "}")
                  )
    ) %>%
    DT::formatStyle(columns = 1, target = 'row',
                backgroundColor = DT::styleRow(list(odd_rownums, even_rownums), c('#D0D0D0','#C0C0C0'))
    ) %>%
    DT::formatStyle(colnames(data_tbl), "white-space"="nowrap")


  if(length(formats_nms$numbers)>0){
    out <- out %>%
      DT::formatStyle(formats_nms$numbers, color = DT::styleInterval(0, c('darkred', 'black')))
  }

  if(length(formats_nms$rates)>0){
    out <- out %>%
      DT::formatPercentage(formats_nms$rates, digits = 2)
  }

  if(length(formats_nms$dollars)>0){
    out <- out %>%
      DT::formatCurrency(formats_nms$dollars, digits = 0, before = F, currency = " $")
  }

  if(length(formats_nms$percents)>0){
    out <- out %>%
      DT::formatPercentage(formats_nms$percents)

  }

  if(length(formats_nms$counts)>0){
    out <- out %>% DT::formatCurrency(formats_nms$counts, digits = 0, currency = "")
  }


  if(has_totals){
    out <- out %>%
      DT::formatStyle(columns = 1, target = 'row',
                  fontSize = DT::styleRow(max_row, '112%'),
                  backgroundColor = DT::styleRow(max_row, 'gray'),
                  backgroundImage = DT::styleRow(max_row, 'linear-gradient(180deg, black 13%, transparent 13%, transparent 84%, black 84%, black 89%, transparent 89%, transparent 93%, black 93% )'),
                  backgroundRepeat = DT::styleRow(max_row, 'no-repeat')

      ) %>%
      DT::formatStyle(1, fontSize = DT::styleRow(max_row, '89%'))
  }
  # replaces callback
  #                   rowCallback=DT::JS(paste0(
  #                     'function(row,data) {
  #                         if($(row)["0"]["_DT_RowIndex"] == ', nrow(dep_sum_tbl)-1 ,
  #                         ') $(row).css({"background-color": "gray",
  #                         "background-image":"linear-gradient(180deg, black 13%, transparent 13%, transparent 84%, black 84%, black 89%, transparent 89%, transparent 93%, black 93%)","background-repeat": "no-repeat", "background-size" : "100% 100%"})
  #                         else if($(row)["0"]["_DT_RowIndex"] % 2 < 1) $(row).css("background","#C0C0C0")
  #                         else $(row).css("background","#D0D0D0")
  #                     }'))

  return(out)

}

