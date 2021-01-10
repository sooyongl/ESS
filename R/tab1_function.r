#' @include tab0_function.r
#'
tab1_group_out <-
  function(inpData, selectedCp, WESS, modal = F){

    # inpData = tab0$est_cs[[1]];selectedCp = tab0$selected_CP[[1]];WESS = T; modal = F
    # inpData = tab1$modal_est_cs[[1]]; selectedCp = tab1$modal_selected_cp[[1]];WESS = input$WESS; modal = F

    num_item <- nrow(inpData)
    total_item <- num_item*length(selectedCp)

    ln_0 <- names(inpData)[(which(names(inpData)=="ALD")+1):(which(names(inpData)=="Operational_Lv")-1)]
    ln_1 <- ln_0[!str_detect(ln_0, "_W")]
    ln_2 <- ln_0[str_detect(ln_0, "_W")]
    target_loc <-  names(inpData)[(which(names(inpData)=="ALD")-1)]

    cor_inc <- inpData %>% pull(Correlation) %>% unique()

    if (modal == F){
      inpData <-
        inpData %>%
        slice(selectedCp) %>%
        select(GCA, Table, Panelist, OOD, Item_ID,
               all_of(target_loc), all_of(ln_0))
    } else {
      inpData <-
        inpData %>%
        slice(selectedCp) %>%
        select(GCA, Table, OOD, Item_ID,
               all_of(target_loc), all_of(ln_0))
    }

    start_level = which(names(inpData)==target_loc) + 1

    d1 <- inpData
    res <-
      foreach(ti = 1:nrow(d1), .combine = 'cbind') %do% {
        # ti <- 1
        ll <- start_level + 1*(ti-1)
        ww <- (start_level+nrow(d1)) + 1*(ti-1)

        d2 <-
          d1[ti, ] %>%
          select(OOD, all_of(target_loc), all_of(ll), all_of(ww))
        names(d2)[1] <- paste0(ln_1[ti],"_p")
        names(d2)[2] <- paste0(ln_1[ti],"_loc")
        return(d2)
      } %>%
      mutate(!!paste0("L","_sum") := eval(parse(text = paste(ln_1, collapse = "+"))),
             !!paste0("LW","_sum") := eval(parse(text = paste(ln_2, collapse = "+")))
      ) %>%
      select(ends_with("_p"), ends_with("_loc"), everything()) %>%
      bind_cols(d1[1,c(1:(4-modal))], .) %>%
      mutate(num_item = num_item, total_item = total_item) %>%
      mutate(Correlation = round(cor_inc,3), .before = OOD)

    return(list(res = res))
  }
#'
#'
tab1_group_out_all <-
  function(inpData, selectedCp, WESS, modal = F){
    # inpData<- tab1$modal_est_cs_all[[1]];
    # selectedCp <- tab1$modal_selected_cp_all[[1]]
    # WESS <- input$WESS;

    num_item <- nrow(inpData)
    total_item <- num_item*length(selectedCp)

    ln_0 <-
      names(inpData)[(which(names(inpData)=="ALD")+1):(which(names(inpData)=="Operational_Lv")-1)]
    ln_1 <- ln_0[!str_detect(ln_0, "_W")]
    ln_2 <- ln_0[str_detect(ln_0, "_W")]
    target_loc <-  names(inpData)[(which(names(inpData)=="ALD")-1)]

    cor_inc <- inpData %>% pull(Correlation) %>% unique()

    inpData <-
      inpData %>%
      slice(selectedCp) %>%
      select(GCA, -Round, OOD, Item_ID,
             all_of(target_loc), all_of(ln_0))

    start_level = which(names(inpData)==target_loc) + 1

    d1 <- inpData
    res <-
      foreach(ti = 1:nrow(d1), .combine = 'cbind') %do% {
        # ti <- 1
        ll <- start_level + 1*(ti-1)
        ww <- (start_level+nrow(d1)) + 1*(ti-1)

        d2 <-
          d1[ti, ] %>%
          select(OOD, all_of(target_loc), all_of(ll), all_of(ww))
        names(d2)[1] <- paste0(ln_1[ti],"_p")
        names(d2)[2] <- paste0(ln_1[ti],"_loc")
        return(d2)
      } %>%
      mutate(!!paste0("L","_sum") := eval(parse(text = paste(ln_1, collapse = "+"))),
             !!paste0("LW","_sum") := eval(parse(text = paste(ln_2, collapse = "+")))
      ) %>%
      select(ends_with("_p"), ends_with("_loc"), everything()) %>%
      bind_cols(d1[1,c(1:2)], .) %>%
      mutate(num_item = num_item, total_item = total_item) %>%
      mutate(Correlation = cor_inc, .before = OOD)

    return(list(res = res))
  }

gen_median_table_all <- function(tab1Res){
  # tab1Res <- tab1$res
  tt1 <-
    tab1Res %>%
    group_split(GCA)

  page_name <- names(tab1Res)[str_detect(names(tab1Res), "_p")]

  get_med <- function(given){
    length_given <- length(given)
    middle <- length_given/2
    middle_point <- round(middle,0)
    if(length_given %% 2 == 0){
      given_med <- sort(given)[middle_point]
    } else {
      given_med <- sort(given)[c(middle_point,(middle_point+1))]
    }
    return(given_med)
  }

  med_cutscore <-
    foreach(mei = 1:length(tt1), .combine = 'rbind') %do% {
      # mei = 1; pi = 2
      dataUse <- tt1[[mei]]
      num_level <- length(page_name)
      loc_nm <- names(dataUse)[str_detect(names(dataUse), "_loc")]
      foreach(pi = 1:length(page_name), .combine = 'cbind') %do% {
        # lv_nm <- unlist(map(str_split(page_name, "_p"), ~.x[1]))
        # weight_nm <- str_replace(page_name, "_p", "_W")
        # level_start <- lv_nm[pi]
        # weight_start <- weight_nm[pi]

        pg_start <- page_name[pi]
        loc_start <- loc_nm[pi]

        for_given <- tt1[[mei]][[page_name[pi]]]

        medp <- get_med(for_given)
        mp <- c()
        for(temi in 1:length(medp)) {
          # temi = 2
          med_point <- which(tt1[[mei]][[page_name[pi]]] %in% medp[temi] )
          mp[temi] <- med_point[1]
        }

        dataUse %>%
          select(all_of(pg_start), all_of(loc_start)) %>%
          slice(mp) %>%
          summarise_all( mean )
      }
    }

  median_table <-
    tab1Res %>%
    distinct(GCA) %>%
    mutate(Table = 0) %>%
    bind_cols(.,med_cutscore)

  return(median_table)
}
#
estCutScore_mode_all <- function(data, information) {

  gcaid <- information$data_ready$id_list$GCA
  cond <- crossing(gcaid)
  inploc <- information$base_data$loc_nm

  lvnm <- information$data_ready$level_nm
  locReady <- information$data_ready$location_ready
  WESS <- information$base_data$WESS

    mod_data_1 <- data %>% bind_rows()

    modal_ALD <-
      foreach(i = 1:nrow(cond)) %do% {
        # i <- 1
        ext_ALD <-
          mod_data_1 %>%
          filter(GCA == cond[i,1] %>% pull()) %>%
          group_split(Panelist) %>%
          map(., ~ .x$ALD) %>%
          set_names(., nm = 1:length(.)) %>%
          bind_cols()

        ext_cors <-
          mod_data_1 %>%
          filter(GCA == cond[i,1] %>% pull()) %>%
          distinct(Panelist, Correlation) %>% pull(Correlation)

        tabl_ALD <- apply(ext_ALD, 1, get_mode, ext_cors)
      }

    mod_data_2 <-
      mod_data_1 %>%
      distinct(GCA, Round, Item_ID, OOD, !!as.name(inploc)) %>%
      mutate(ALD = unlist(modal_ALD))
  ######################################################################
    split_filter <-
      mod_data_2 %>%
      group_split(GCA) %>%
      map(., ~ .x %>% select(-OOD, -all_of(inploc)))

    mode_cs <-
      map(split_filter, estCutScore,
          information
      )
    return(mode_cs)
}
#
gen_median_table <- function(tab1Res){
  # tab1Res <- tab1$res
  tt1 <-
    tab1Res %>%
    group_split(GCA, Table)

  page_name <- names(tab1Res)[str_detect(names(tab1Res), "_p")]

  get_med <- function(given){
    length_given <- length(given)
    middle <- length_given/2
    middle_point <- round(middle,0)
    if(length_given %% 2 == 0){
      given_med <- sort(given)[middle_point]
    } else {
      given_med <- sort(given)[c(middle_point,(middle_point+1))]
    }
    return(given_med)
  }

  med_cutscore <-
    foreach(mei = 1:length(tt1), .combine = 'rbind') %do% {
      # mei = 1; pi = 2
      dataUse <- tt1[[mei]]
      num_level <- length(page_name)
      loc_nm <- names(dataUse)[str_detect(names(dataUse), "_loc")]
      foreach(pi = 1:length(page_name), .combine = 'cbind') %do% {
        # lv_nm <- unlist(map(str_split(page_name, "_p"), ~.x[1]))
        # weight_nm <- str_replace(page_name, "_p", "_W")
        # level_start <- lv_nm[pi]
        # weight_start <- weight_nm[pi]

        pg_start <- page_name[pi]
        loc_start <- loc_nm[pi]

        for_given <- tt1[[mei]][[page_name[pi]]]

        medp <- get_med(for_given)
        mp <- c()
        for(temi in 1:length(medp)) {
          # temi = 2
          med_point <- which(tt1[[mei]][[page_name[pi]]] %in% medp[temi] )
          mp[temi] <- med_point[1]
        }

        dataUse %>%
          select(all_of(pg_start), all_of(loc_start)) %>%
          slice(mp) %>%
          summarise_all( mean )
      }
    }

  median_table <-
    tab1Res %>%
    distinct(GCA, Table) %>%
    bind_cols(.,med_cutscore)

  return(median_table)
}
#
gen_median_output <- function(medTable) {

  page_name <- names(medTable)[str_detect(names(medTable), "_p")]
  panel.key <- medTable[, 1:2]
  table.inf <- medTable[, -c(1:2)]
  table.keep <- table.inf
  level_names <- names(table.inf)

  for(mi in 1:length(page_name)) {
    # mi <- 2
    mii <- 2 + 2*(mi-1)
    miii <- 1 + 2*(mi-1)
    mut.inp <- glue::glue(
      'paste0({level_names[{miii}]}," (",{level_names[{mii}]},")")'
    )
    table.inf <- table.inf %>% mutate(!!page_name[mi] := eval(parse(text = mut.inp)))
  }
  median_out <-
    table.inf %>% select(-ends_with("_loc")) %>%
    bind_cols(panel.key, .) %>%
    mutate(Table = as.character(Table),
      Table = if_else(Table == "0", "All", Table))
  return(median_out)
}
#
estCutScore_mode <- function(data, information){
  # data = est_cutscore;information
  gcaid <- information$data_ready$id_list$GCA
  cond <- information$data_ready$id_list$Table
  inploc <- information$base_data$loc_nm

  lvnm <- information$data_ready$level_nm
  locReady <- information$data_ready$location_ready
  WESS <- information$base_data$WESS

  mod_data_1 <- data %>% bind_rows()

  modal_ALD <-
    foreach(i = 1:nrow(cond)) %do% {
      # i <- 1
      ext_ALD <-
        mod_data_1 %>%
        filter(GCA == cond[i,1] %>% pull(),
               Table == cond[i,2] %>% pull()) %>%
        group_split(Panelist) %>%
        map(., ~ .x$ALD) %>%
        set_names(., nm = 1:length(.)) %>%
        bind_cols()

      ext_cors <-
        mod_data_1 %>%
        filter(GCA == cond[i,1] %>% pull(),
               Table == cond[i,2] %>% pull()) %>%
        distinct(Panelist, Correlation) %>% pull(Correlation)

      tabl_ALD <- apply(ext_ALD, 1, get_mode, ext_cors)
    }

  mod_data_2 <-
    mod_data_1 %>%
    distinct(GCA, Round, Table, Item_ID, OOD, !!as.name(inploc)) %>%
    mutate(ALD = unlist(modal_ALD))
  ###########################################################################
  ###########################################################################
  split_filter <-
    mod_data_2 %>%
    group_split(GCA, Table) %>%
    map(., ~ .x %>% select(-OOD, -all_of(inploc)))

  mode_cs <-
    map(split_filter, estCutScore,
        information
    )
  return(mode_cs)
}
#
get_mode <- function(x, corInf){
  # x <- ext_ALD[1,]
  # corInf <- ext_cors
  x <- x %>% unlist()

  cor_order <-
    data.frame(x, corInf) %>%
    arrange(., desc(corInf))

  a1 <- x %>% unlist()
  a2 <- sort(table(a1), decreasing = T)

  if(sum(a2 == max(a2)) > 1){

    max_name <- names(which(a2 == max(a2)))

    a2 <-
      cor_order %>%
      filter(x %in% max_name) %>%
      .[1,1]
  } else {
    modal_ALD <- names(a2)[1]
  }
}
#
estCutScore_mode_manual <- function(data, information, manual_cutpoint) {

  gcaid <- information$data_ready$id_list$GCA
  cond <- crossing(gcaid)
  inploc <- information$base_data$loc_nm

  lvnm <- information$data_ready$level_nm
  locReady <- information$data_ready$location_ready
  WESS <- information$base_data$WESS

  mod_data_1 <- data %>% bind_rows()

  modal_ALD <-
    foreach(i = 1:nrow(cond)) %do% {
      # i <- 1
      ext_ALD <-
        mod_data_1 %>%
        filter(GCA == cond[i,1] %>% pull()) %>%
        group_split(Panelist) %>%
        map(., ~ .x$ALD) %>%
        set_names(., nm = 1:length(.)) %>%
        bind_cols()

      ext_cors <-
        mod_data_1 %>%
        filter(GCA == cond[i,1] %>% pull()) %>%
        distinct(Panelist, Correlation) %>% pull(Correlation)

      tabl_ALD <- apply(ext_ALD, 1, get_mode, ext_cors)
    }

  mod_data_2 <-
    mod_data_1 %>%
    distinct(GCA, Round, Item_ID, OOD, !!as.name(inploc)) %>%
    mutate(ALD = unlist(modal_ALD))
  ######################################################################
  split_filter <-
    mod_data_2 %>%
    group_split(GCA) %>%
    map(., ~ .x %>% select(-OOD, -all_of(inploc)))

  mode_cs <-
    map2(split_filter, manual_cutpoint, estCutScore_manual,
         information
    )
  return(mode_cs)
}
#'
estCutScore_manual <- function(inpData, manual_cutpoint, information) {
  # inpData = information$split_data[[1]];
  # inpData = split_filter[[1]];
  locInf <- information$data_ready$location_ready
  levelNm_list <- information$data_ready$level_nm
  WESS <- information$base_data$WESS_nm

  SD_data <- information$data_ready$SD_data
  threshold_data <- information$base_data$threshold

  GCA_data <- inpData

  need_data <- data_prep(GCA_data, levelNm_list, locInf)
  for(ai in seq_along(names(need_data))) {
    assign(names(need_data)[ai], need_data[[ai]])
  }
  locnm <- names(location)[3]
  ald_vector <- remove_blank_vector(data_1 %>% pull(ALD) )
  lv_vector <- remove_blank_vector(level_nm)

  SD_inp <- SD_data %>% filter(GCAid == test_id) %>% pull(SD)
  #est Cut page
  cut_scores <-
    cal_cs(lv_vector, ald_vector, location, threshold_data)

  cutPoint <- cal_minp(cut_scores)
  selected_CP <- manual_cutpoint

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
}
