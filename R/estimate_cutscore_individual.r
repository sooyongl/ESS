estCutScore <- function(information) {
  #
  est_cutscore <-
    foreach(inpData = information$split_data) %do% {

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
# estCutScore <- function(inpData, information) {
#   # inpData = information$split_data[[1]];
#
#   GCA_data <- inpData
#
#   locInf <- information$data_ready$location_ready
#   levelNm_list <- information$data_ready$level_nm
#   WESS <- information$base_data$WESS
#
#   SD_data <- information$data_ready$SD_data
#   threshold_data <- information$base_data$threshold
#
#   need_data <- data_prep(GCA_data, levelNm_list, locInf)
#   for(ai in seq_along(names(need_data))) {
#     assign(names(need_data)[ai], need_data[[ai]])
#   }
#   locnm <- names(location)[3]
#   ald_vector <- remove_blank(data_1 %>% pull(ALD) )
#   lv_vector <- remove_blank(level_nm)
#
#   SD_inp <- SD_data %>% filter(GCAid == test_id) %>% pull(SD)
#   #est Cut page
#   cut_scores <-
#     cal_cs(lv_vector, ald_vector, location, threshold_data)
#
#   cutPoint <- cal_minp(cut_scores)
#   selected_CP <- select_cp(cutPoint, cut_scores, WESS)
#
#   cutPoint$weight <- round(cutPoint$weight / SD_inp, 2)
#
#   data_2 <-
#     cut_scores %>%
#     bind_cols(data_1, bind_loc, .) %>%
#     relocate(., OOD, !!as.name(locnm), .after = Item_ID)
#
#   op_num <- rep(0, nrow(inpData))
#   op_num[ selected_CP  ] <- 1
#   Operational_name <- get_opname1(inpData, lv_vector, op_num)
#
#   loc_num <- data_2 %>% pull(locnm)
#   ald_num <- match(ald_vector, lv_vector)
#
#   cor_inc <- cor(ald_num, loc_num)
#
#   data_3 <-
#     data_2 %>%
#     mutate(
#       Operational_Lv = Operational_name,
#       Correlation = cor_inc
#     ) %>%
#     mutate_at(vars(ends_with("_W")), ~ round(./SD_inp,2))
#   return(list(est_cs = data_3, est_cp = cutPoint,selected_CP = selected_CP))
# }
#'
cal_cs <-
  function(lvVec, aldVec, loc_data, threshold){
    # lvVec=lv_vector;aldVec=ald_vector;loc_data=location
    # lvVec = c("Level1","Level2","Level3")
    # aldVec = c("Level1","Level1","Level1","Level2","Level2","Level3","Level2","Level1","Level3","Level3")

    nlv <- 1:length(lvVec)
    ald_match <- match(aldVec, lvVec)

    er <- length(aldVec)
    loc <- loc_data %>% pull(3)

    if(threshold) {
      cutSC <-
        foreach(n = 2:(length(lvVec)), .combine = 'cbind') %do% {
          foreach(cr = 1:er, .combine = 'rbind') %do% {
            # n = 2 ; cr = 5
            cr_abo <- cr + 1
            cr_bel <- cr
            if(cr == er){
              cr_abo <- cr
            }

            true_bel <- ald_match[0:(cr)] >= n
            true_abo <- ald_match[(cr_abo):er] < n

            c_bel <- sum(true_bel, na.rm = T)
            c_abo <- sum(true_abo, na.rm = T)

            original <- c_bel + c_abo

            if(cr != er){
              cr_bel <- cr_bel+1
            }

            c_bel <- loc[(cr_bel)] - loc[0:(cr)][true_bel]
            c_abo <- (loc[cr_abo] - 1) - loc[cr_abo:er][true_abo]

            weighted <- sum(abs(c_bel), na.rm = T) + sum(abs(c_abo), na.rm = T)

            c(original, weighted)
          }
        }
    } else {
      cutSC <-
        foreach(n = 2:length(lvVec), .combine = 'cbind') %do% {
          foreach(cr = 1:er, .combine = 'rbind') %do% {
            # n = 2 ; cr = 1
            cr_abo <- cr
            cr_bel <- cr

            true_bel <- ald_match[0:(cr - 1)] >= n
            true_abo <- ald_match[cr_abo:er] < n

            c_bel <- sum(true_bel, na.rm = T)
            c_abo <- sum(true_abo, na.rm = T)

            original <- c_bel + c_abo

            c_bel <- loc[cr_bel] - loc[0:(cr - 1)][true_bel]
            c_abo <- (loc[cr_abo] - 1) - loc[cr_abo:er][true_abo]

            weighted <- sum(abs(c_bel), na.rm = T) + sum(abs(c_abo), na.rm = T)

            c(original, weighted)
          }
        }

    }

    odd <- foreach(i = 1:(ncol(cutSC)/2), .combine = 'c') %do% {1 + 2*(i - 1)}
    even <- foreach(i = 1:(ncol(cutSC)/2), .combine = 'c') %do% {2 + 2*(i - 1)}
    cutSC <- data.frame(cutSC[, c(odd, even)])
    rownames(cutSC) <- NULL
    names(cutSC) <- c(paste0("L",nlv[2:(length(nlv))]),paste0("L",paste0(nlv[2:(length(nlv))],"_W")))

    return(cutSC)
  }
#
cal_minp <- function(dataInp) { # dataInp = cut_scores
  est_lev <- dataInp
  minimun_points <-
    foreach(el = 1:ncol(est_lev)) %do% {
      # el = 1
      which(est_lev[el] == min(est_lev[el]))
    }
  o.ver <- matrix(
    unlist(cross(minimun_points[1:(length(minimun_points)/2)])),
    ncol = (length(minimun_points)/2),
    byrow = T
  ) %>%
    data.frame(.) %>%
    set_names(.,
              names(est_lev)[1:(length(est_lev)/2)])
  w.ver <- matrix(
    unlist(
      cross(
        minimun_points[(length(minimun_points)/2 + 1):length(minimun_points)]
      )
    ),
    ncol = (length(minimun_points)/2),
    byrow = T
  ) %>%
    data.frame(.) %>%
    set_names(.,
              names(est_lev)[
                (length(est_lev)/2 + 1):length(est_lev)
              ]
    )
  return(list(default = o.ver, weight = w.ver))
}
#'
#'
select_cp<-
  function(cutPoint, estCutscore, WESS){
    # cutPoint<-cut_point[[1]];estCutscore<-est_cutscore[[1]];
    # inputWESS<-WESS
    level_names <- map(cutPoint, names)
    l_names <- level_names[["default"]]
    w_names <- level_names[["weight"]]

    data_use_1 <- estCutscore

    if(WESS){
      cutPoint <- cutPoint[["weight"]]
    } else{
      cutPoint <- cutPoint[["default"]]
    }

    cut_candi <- cutPoint

    ppp <-
      foreach(ii = 1:ncol(cut_candi), .combine = 'c') %do% {
        # ii <- 1
        li <- l_names[ii]
        wi <-  w_names[ii]
        cp1 <- unique(unlist(cut_candi[,ii]))

        pp <-
          data_use_1 %>%
          slice(cp1) %>%
          arrange(!!as.name(wi), !!as.name(li)) %>%
          slice(1) %>%
          select(all_of(li), all_of(wi)) %>% data.frame() %>%
          unname() %>% unlist()

        which( data_use_1[[li]] ==  pp[1] & data_use_1[[wi]] ==  pp[2])[1]

      }
    return(ppp = ppp)
  }
#'
#'
get_opname1 <- function(datainp, lvname, opnum){

  efficacy_data_1 <- datainp # efficacy_data_1 <- est_cutscore[[i]]
  eff_name <- names(efficacy_data_1)
  target_filter = "ALD"

  item_start <- 1
  cut_point <- which(opnum==1)
  num_item <-length(opnum)

  operational_1 <- c(item_start, cut_point, num_item)
  Operational_name <- rep(lvname[length(lvname)], length(opnum))

  for(i in 1:(length(cut_point)+1)){
    # i <- 2
    if(i == (length(cut_point)+1)) {
      Operational_name[operational_1[i]:(operational_1[(i+1)])] <- lvname[i]

    } else {
      Operational_name[operational_1[i]:(operational_1[(i+1)]-1)] <- lvname[i]
    }
  }

  return(Operational_name)
}
#'
gen_indi_table <- function(tab1Res) {
  # tab1Res <- tab1$res
  page_name <- names(tab1Res)[str_detect(names(tab1Res), "_p")]
  weight_name <- names(tab1Res)[str_detect(names(tab1Res), "_W")]
  default_name <-
    str_split(weight_name, "_") %>% unlist() %>% .[!. %in% "W"]

  cors_p <- which(names(tab1Res)=="Correlation")
  panel.key <- tab1Res[, 1:(cors_p)]
  table.inf <- tab1Res[, -c(seq_len(ncol(panel.key)))]
  table.keep <- table.inf
  level_names <- names(table.inf)

  for(mi in 1:length(page_name)) {
    # mi <- 1
    mii <- mi + length(page_name)

    mut.inp <- glue::glue(
      'paste0({level_names[{mi}]}," (",{level_names[{mii}]},")")'
    )

    table.inf <- table.inf %>% mutate(!!page_name[mi] := eval(parse(text = mut.inp)))
  }

  table.inf <- table.inf %>% select(-ends_with("_loc"))

  for(mi in 1:length(default_name)) {
    # mi <- 2
    mii <- mi + length(default_name)

    mut.inp <- glue::glue(
      'paste0({default_name[{mi}]}," / ",num_item)'
    )

    table.inf <- table.inf %>% mutate(!!default_name[mi] := eval(parse(text = mut.inp)))
  }
  table.inf <-
    table.inf %>%
    mutate(L_sum = paste0(L_sum, " / ", total_item)) %>%
    select(-num_item, -total_item)

  tbl_res <- bind_cols(panel.key, table.inf) %>%
    mutate(Table = as.character(Table),
      Table = if_else(Table == "0", "All", Table))

  return(tbl_res)
}

# Helper functions
#------------------
#'
#'
data_prep <- function(inpData, lvNm, locInf){
  # fourthData <- fourth_data
  GCAId <- inpData %>% pull(GCA) %>% unique()
  level_nm <- lvNm %>% select(all_of(GCAId)) %>% pull()

  location <- locInf$location[[GCAId]]
  bind_loc <- locInf$bind_loc[[GCAId]]

  return(list(
    data_1 = inpData,
    level_nm = level_nm,
    test_id = GCAId,
    location = location,
    bind_loc = bind_loc))
}
