#' @include tab_function.r
#' run_ESS
#' @export
run_ESS <- function(filePath, grade, ald, location, WESS, modal, threshold) {

  res_ess <- new("ess")

  data_list <- data_ready(filePath)
  ####################################################
  res_ess@information <-
    get_data_info(
      data_list,
      grade = grade,
      ald = ald,
      location = location,
      WESS = WESS,
      modal = modal,
      threshold = threshold
    )
  res_ess@tab0 <- gen_tab0(res_ess@information)
  res_ess@tab1 <- gen_tab1(res_ess@tab0, res_ess@information)
  res_ess@tab2 <- gen_tab2(res_ess@tab1, res_ess@information)
  res_ess@tab3 <- gen_tab3(res_ess@tab1, res_ess@information)
  res_ess@tab4 <- gen_tab4(res_ess@tab1, res_ess@tab2, res_ess@tab3, res_ess@information)

  return(res_ess)
}
#' run_ESS
# update_ESS <- function(res_ess, manial_cp) {
#
#   # need to be fixed for update
#   # tab1 <- update_tab1(tab0, information, manual_cp)
#   # tab2 <- update_tab2(tab1, information)
#
#   return(res_ess)
# }
