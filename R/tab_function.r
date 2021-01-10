#' @include tab4_function.r

gen_tab0 <- function(information) {
  #
  data <- information$split_data
  est_cutscore <-
    foreach(inpData = data) %do% {

      GCA_data <- inpData

      locInf <- information$data_ready$location_ready
      levelNm_list <- information$data_ready$level_nm
      WESS <- information$base_data$WESS

      SD_data <- information$data_ready$SD_data
      threshold_data <- information$base_data$threshold

      need_data <- data_prep(GCA_data, levelNm_list, locInf)
      for(ai in seq_along(names(need_data))) {
        assign(names(need_data)[ai], need_data[[ai]])
      }
      locnm <- names(location)[3]
      ald_vector <- remove_blank(data_1 %>% pull(ALD) )
      lv_vector <- remove_blank(level_nm)

      SD_inp <- SD_data %>% filter(GCAid == test_id) %>% pull(SD)
      #est Cut page
      cut_scores <-
        cal_cs(lv_vector, ald_vector, location, threshold_data)

      cutPoint <- cal_minp(cut_scores)
      selected_CP <- select_cp(cutPoint, cut_scores, WESS)

      cutPoint$weight <- round(cutPoint$weight / SD_inp, 2)

      data_2 <-
        cut_scores %>%
        bind_cols(data_1, bind_loc, .) %>%
        relocate(., OOD, !!as.name(locnm), .after = Item_ID)

      op_num <- rep(0, nrow(inpData))
      op_num[ selected_CP  ] <- 1
      Operational_name <- get_opname1(inpData, lv_vector, op_num)

      loc_num <- data_2 %>% pull(locnm)
      ald_num <- match(ald_vector, lv_vector)

      cor_inc <- cor(ald_num, loc_num)

      data_3 <-
        data_2 %>%
        mutate(
          Operational_Lv = Operational_name,
          Correlation = cor_inc
        ) %>%
        mutate_at(vars(ends_with("_W")), ~ round(./SD_inp,2))
      return(list(est_cs = data_3, est_cp = cutPoint,selected_CP = selected_CP))

    } %>%
    set_names(.,
              nm = information$data_ready$id_list[["PanelID"]] %>%
                filter(GCA %in% information$data_ready$id_list$GCA) %>%
                pull(3) %>% unique()
    )

  est_cs <- map(est_cutscore, ~ .x$est_cs)
  est_cp <- map(est_cutscore, ~ .x$est_cp)
  selected_CP <- map(est_cutscore, ~ .x$selected_CP)

  tab0 <- list(est_cs = est_cs, est_cp = est_cp, selected_CP = selected_CP)

  return(tab0)
}
#
gen_tab1 <- function(tab0, information){

  tab1 <- list()
  # tab1$res <- tab1_group_out(tab0, information, modal = F)
  tab1$res <-
    map2(tab0$est_cs, tab0$selected_CP,
         tab1_group_out, information$base_data$WESS,
         modal = F
    ) %>%
    map(., ~ .x$res) %>%
    bind_rows() %>%
    select(-OOD)

  tab1$indi_table <- gen_indi_table(tab1$res)

  # For Median Table ready
  tab1$median_res <- gen_median_table(tab1$res)
  tab1$median_res_all <- gen_median_table_all(tab1$res)
  tab1$median_res_com <-
    bind_rows(tab1$median_res, tab1$median_res_all) %>%
    arrange(GCA)

  # For Median Table output ready
  tab1$median_table <- gen_median_output(tab1$median_res_com)
  ###############################################################3
  # modal Cut Score Estimate
  est_cutscore <- tab0$est_cs

  tab1$modal_est_cutscore <-
    estCutScore_mode(est_cutscore, information)

  tab1$modal_est_cutscore_all <-
    estCutScore_mode_all(est_cutscore, information)

  tab1$modal_est_cs <- map(tab1$modal_est_cutscore, ~ .x$est_cs)
  tab1$modal_est_cp <- map(tab1$modal_est_cutscore, ~ .x$est_cp)
  tab1$modal_selected_cp <- map(tab1$modal_est_cutscore, ~ .x$selected_CP)

  tab1$modal_est_cs_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cs)
  tab1$modal_est_cp_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cp)
  tab1$modal_selected_cp_all <-
    map(tab1$modal_est_cutscore_all, ~ .x$selected_CP)

  tab1$modal_res <- # tab1_group_out(tab1, information, modal = T)
    map2(tab1$modal_est_cs, tab1$modal_selected_cp,
         tab1_group_out,
         information$base_data$WESS,
         modal = T
    ) %>%
    map(., ~ .x$res) %>%
    bind_rows() %>%
    select(-OOD)


  tab1$modal_res_all <-
    map2(tab1$modal_est_cs_all, tab1$modal_selected_cp_all,
         tab1_group_out_all, information$base_data$WESS,
         modal = T
    ) %>%
    map(., ~ .x$res) %>%
    bind_rows() %>%
    select(-OOD) %>%
    mutate(Table = 0, .after = GCA)

  tab1$modal_res_com <-
    bind_rows(tab1$modal_res, tab1$modal_res_all) %>%
    arrange(GCA)

  # modal cut score ouput ready
  tab1$modal_table <- gen_indi_table(tab1$modal_res_com)
  tab1$modal_table_all <- gen_indi_table(tab1$modal_res_all)

  return(tab1)
}
#'
gen_tab2 <- function(tab1, information) {

  tab2 <- list()

  modal_est_cs <- tab1$modal_est_cs_all
  modal_est_cp <- tab1$modal_est_cp_all
  modal_selected_cp <- tab1$modal_selected_cp_all

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))

  est_cutscore <- modal_est_cs
  target_filter <- information$base_data$target_nm

  tab2$for_tab2_out <-
    # put the results into each output
    lapply(1:length(n.of.gca), function(vi) {
      # vi = 1 ; vvi = 1
      in_num <- n.of.tb[vi]
      lapply(1:in_num, function(vvi) {

        dataUse_0 <- est_cutscore[vi][[vvi]]

        eff_data <-
          tab1$modal_table_all[vi,] %>%
          data.frame() %>%
          mutate(Correlation = round(Correlation, 3))

        dataUse_1 <- dataUse_0 %>% select(-Correlation)

        ct_data <- dataUse_0 %>% select(ALD, Operational_Lv)
        ct_1 <-
          xtabs(
            as.formula(paste0("~","Operational_Lv","+","ALD")), ct_data
          )

        dataUse_2 <- tab2_plot_data(dataUse_1, information, vi)

        p1 <- tab2_plot(dataUse_2, information$base_data$WESS)

        list(eff_data = eff_data, t_out = dataUse_1,
             crosst = ct_1, p1 = p1)
      })
    })


  return(tab2)
}
#'
#'
update_tab2 <- function(tab0, information, manual_cp) {

  tab1 <- list()
  est_cutscore <- tab0$est_cs

  tab1$modal_est_cutscore_all <-
    estCutScore_mode_manual(est_cutscore, information, manual_cp)

  tab1$modal_est_cs_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cs)
  tab1$modal_est_cp_all <- map(tab1$modal_est_cutscore_all, ~ .x$est_cp)
  tab1$modal_selected_cp_all <- map(tab1$modal_est_cutscore_all, ~ .x$selected_CP)

  tab1$modal_res_all <-
    map2(tab1$modal_est_cs_all, tab1$modal_selected_cp_all,
         tab1_group_out_all, input$WESS, modal = T) %>%
    map(., ~.x$res) %>% bind_rows() %>%
    select(-OOD) %>%
    mutate(Table = 0, .after = GCA)

  tab1$modal_table_all <- gen_indi_table(tab1$modal_res_all)

  modal_est_cs <- tab1$modal_est_cs_all
  modal_est_cp <- tab1$modal_est_cp_all
  modal_selected_cp <- tab1$modal_selected_cp_all

  est_cutscore <- modal_est_cs
  target_filter <- information$base_data$target_nm


  tab2 <- gen_tab2(tab1, information)
  return(tab2)
}

#'

gen_tab3 <- function(tab1, information) {

  tab3 <- list()

  target_filter <- information$base_data$target_nm
  target_loc <- information$base_data$loc_nm
  gca_nm <- information$data_ready$id_list$GCA

  setup_data <- information$imported_data$setup_data
  examinee_data <-
    information$imported_data$examinee_data

  gca_p <-
    which(remove_blank(names(examinee_data)) ==
            remove_blank("grade")|
            remove_blank(names(examinee_data)) ==
            remove_blank("gca"))

  examinee_data <-
    examinee_data %>%
    filter(GCA %in% gca_nm) %>%
    group_split(!!as.name(names(examinee_data)[gca_p])) %>%
    set_names(., nm = gca_nm)

  item_data <-
    information$imported_data$item_data %>%
    filter(GCA %in% gca_nm) %>%
    group_split(GCA) %>%
    set_names(., nm = gca_nm)

  num_item <- map(item_data, nrow)
  loc_num <- map(item_data, ~ .x %>% select(all_of(target_loc)))

  if(information$base_data$modal == "modal") {
    dataUse_0 <- tab1$modal_res_all

    data_name <- names(dataUse_0)
    page_name <- data_name[str_detect(data_name, "_p")]
    loc_name <- data_name[str_detect(data_name, "_loc")]
    level_names <- paste0("Level", 1:(length(page_name)+1))
    cut_data <- dataUse_0 %>% select(matches("_p|_loc"))

    dataUse_0 <- dataUse_0 %>% mutate_at(page_name,  ceiling)
  } else {
    dataUse_0 <- tab1$median_res_all

    data_name <- names(dataUse_0)
    page_name <- data_name[str_detect(data_name, "_p")]
    loc_name <- data_name[str_detect(data_name, "_loc")]
    level_names <- paste0("Level", 1:(length(page_name)+1))
    cut_data <- dataUse_0 %>% select(matches("_p|_loc"))

    dataUse_0 <- dataUse_0 %>% mutate_at(page_name,  ceiling)
  }

  tab3$page_data <-
    map(1:nrow(dataUse_0), gen_page_data,
        dataUse_0, item_data, examinee_data, page_name,
        target_loc, level_names)

  tab3$scale_scores <- map(tab3$page_data, ~ .x$scale_scores) %>% bind_rows()
  tab3$perc_ins <- map(tab3$page_data, ~ .x$perc_ins) %>% bind_rows()
  tab3$perc_atabos <- map(tab3$page_data, ~ .x$perc_atabos) %>% bind_rows()
  tab3$perc_bel <- map(tab3$page_data, ~ .x$perc_bel) %>% bind_rows()

  tab3$perc_ins <-
    tab3$perc_ins %>% mutate(perIn_c = paste0(percIn, "%"))
  tab3$perc_atabos <-
    tab3$perc_atabos %>% mutate(percAtabo_c = paste0(percAtabo, "%"))
  tab3$perc_bel <-
    tab3$perc_bel %>% mutate(percBel_c = paste0(percBel, "%"))

  blank_page <-
    map(gca_nm, gen_blank_page, information$data_ready$level_nm[[1]])
  perIn <- tab3$perc_ins %>% group_split(GCA)
  percAtabv <- tab3$perc_atabos %>% group_split(GCA)
  percBel <- tab3$perc_bel %>% group_split(GCA)

  min_data <- cut_data %>% select(ends_with("_p"))

  tab3$eff_page <-
    map(1:length(blank_page), summarize_page,
        blank_page, perIn, percAtabv, percBel, min_data, level_names, num_item,
        loc_num) %>%
    bind_rows(.)

  return(tab3)
}
#'
gen_tab4 <- function(tab1, tab2, tab3, information){

  tab4 <- list()

  n.of.gca <- information$data_ready$id_list$GCA
  n.of.tb <- rep(1, length(n.of.gca))

  loc_nm <- information$base_data$loc_nm

  for_tab2_out <- tab2$for_tab2_out

  level_names <- names(tab3$eff_page)[-c(1:3)]

  SD <- information$data_ready$SD_data

  cs_inf <-
    tab1$modal_res_all %>%
    select(GCA, ends_with("_loc")) %>%
    set_names(., nm = c("GCA", level_names)) %>%
    gather(., "ALD", "Cut_Score", -GCA)

  item_review_table <-
    foreach(vi = 1:length(n.of.gca), .combine = 'rbind') %do% {
      # vi = 2

      cutpoint_inp <- tab1$modal_selected_cp_all[[vi]]

      dataUse_1 <- for_tab2_out[[vi]][[1]][["t_out"]]

      dataUse_Weight <-
        dataUse_1 %>%
        mutate(
          weightSum = dplyr::select(., ends_with("_W")) %>%
            rowSums()
        ) %>% pull(weightSum)

      target_names <- dataUse_1$ALD

      lv_name <- target_names %>% unique() %>% sort()
      #
      given_n <- c(1:length(lv_name))
      targets_n <- match(target_names, lv_name)
      ## dataUse_1["ALD"]
      er <- length(target_names)
      review_table <-
        foreach(n = 2:(length(given_n)), .combine = 'rbind') %do% {
          # n = 2
          cr <- cutpoint_inp[(n-1)]
          # cr = 3
          cr_abo <- cr
          cr_bel <- cr

          c_bel <- which(targets_n[0:(cr - 1)] >= n)
          c_abo <- (which(targets_n[cr_abo:er] < n) - 1) + cr_abo

          dataUse_1[c(c_bel, c_abo), ]
        }
      all_names <- names(review_table)
      review_table <- review_table %>% distinct(!!!syms(all_names))
      review_table <-
        review_table %>%
        select(-Round) %>%
        mutate(.,
               Weight = dplyr::select(., ends_with("_W")) %>%
                 rowSums()
        )
    }

  cs_inf_upper <-
    cs_inf %>%
    mutate(
      ALD = extract_num(ALD),
      ALD = paste0("Level", (ALD - 1))
    ) %>%
    rename(
      "Cut_Score_upper" = "Cut_Score"
    )

  item_review_table <-
    item_review_table %>%
    left_join(., cs_inf, by = c("GCA" = "GCA",
                                "ALD" = "ALD")) %>%
    left_join(., cs_inf_upper, by = c("GCA" = "GCA",
                                      "ALD" = "ALD")) %>%
    left_join(., SD, by = c("GCA" = "GCAid")) %>%

    mutate(AN = extract_num(ALD),
           ON = extract_num(Operational_Lv),
           Diff_LV = AN - ON,
           Distance = case_when(
             AN > ON ~ !!as.name(loc_nm) - Cut_Score,
             AN < ON ~ !!as.name(loc_nm) - (Cut_Score_upper - 1)
           ),
           `Std. Distance` = round(Distance / SD, 3)
    ) %>%

    select(-matches("L[[:digit:]]+"), -AN, -ON) %>%
    select(
      GCA, Item_ID, OOD,
      ALD, Operational_Lv, Diff_LV,
      starts_with("Loc"),
      Distance,`Std. Distance`) %>%
    rename(
      "Aligned_ALD" = "ALD"
    ) %>%
    arrange(
      GCA, Item_ID
    )

  tab4$for_tab4_out <- item_review_table

  return(tab4)
}

