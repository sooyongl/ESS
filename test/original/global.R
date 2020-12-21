# Required Packages
library(shiny);
library(shinythemes);
library(shinyWidgets);
library(tidyverse);
library(glue);
library(foreach);
library(stringi);
library(DT);
library(knitr);
library(kableExtra);
library(readxl);
library(officer);
library(flextable);
library(shinycssloaders);
library(waiter)
#####################################################################
#
# if(Sys.getenv('SHINY_PORT') != "") options(shiny.maxRequestSize=10000*1024^2)
#
tabStyle <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"
#
dataReady_setup <- function(imprt_data) {
  lv_p <-
    which(upper_remove_blank(names(imprt_data[["Setup"]])) ==
            upper_remove_blank("Level Options"))
  level_opt_name <- names(imprt_data[["Setup"]])[lv_p]

  lv_vec <-
    map(str_split(imprt_data[["Setup"]][[lv_p]], ","),remove_blank_vector)

  lv_vec1 <-lv_vec %>%
    lapply(., function(x) sapply(1:length(x), function(xx) paste0("Level",xx) )) %>%
  map(., ~paste(.x, collapse = ", ")) %>%
  do.call("rbind", .)

  first_data <- imprt_data[["Setup"]]
  first_data[[level_opt_name]] <-  lv_vec1
  first_data %>% arrange(1)
  setup_data  <- first_data %>% arrange(1)
  return(setup_data)
}
#
dataReady_rating <- function(imprt_data) {
  ald_p <-
    which(upper_remove_blank(names(imprt_data[["Ratings"]])) ==
            upper_remove_blank("ald"))
  ald_opt_name <- names(imprt_data[["Ratings"]])[ald_p]

  panel_p <- which(
    str_detect(
      upper_remove_blank(names(imprt_data[["Ratings"]])),
      paste0(upper_remove_blank("use"), "|", upper_remove_blank("panel"))
    )
  )
  lv_p <-
    which(upper_remove_blank(names(imprt_data[["Setup"]])) ==
            upper_remove_blank("Level Options"))
  lv_vec <-
    map(str_split(imprt_data[["Setup"]][[lv_p]], ","),remove_blank_vector)

  new_level_name <-
    imprt_data[["Ratings"]] %>%
    mutate(ALD = remove_blank_vector(ALD)) %>%
    group_split(GCA) %>%
    map(., ~ .x %>% pull(ALD)) %>%
    map2(., lv_vec, ~ paste0("Level", match(.x, .y))) %>%
    unlist(.)

  third_data <- imprt_data[["Ratings"]]

  names(third_data)[panel_p] <- "Panelist"

  third_data[[ald_p]] <- new_level_name
  rating_data <- third_data
  return(rating_data)
}
#
dataReady_itemdata <- function(imprt_data){
  item_data <- imprt_data[["ItemMetaData"]]  %>% arrange(1,2)
  return(item_data)
}
#
dataReady_examineedata <- function(imprt_data){
  fifth_data <- fifth_reorg(imprt_data[["ExamineeMetaData"]])
  gca_p <-
    which(upper_remove_blank(names(fifth_data)) ==
            upper_remove_blank("grade")|
            upper_remove_blank(names(fifth_data)) ==
            upper_remove_blank("gca"))

  names(fifth_data)[gca_p] <- "GCA"
  examinee_data <- fifth_data
  return(examinee_data)
}
#
pull_unique <- function(inpData, colNum) {
  inpData %>% pull(colNum) %>% unique()
}
#
data_filter <- function(thirdData, filterV){
  thirdData %>%
    filter(GCA %in% filterV)
}
#
fifth_reorg <-
  function(inpdata){
    if(sum(str_detect(toupper(names(inpdata)), toupper("freq"))) == 0){

      return(inpdata)

    } else {
      inpdata_reorg <- vector("list", ncol(inpdata))

      for(i in 1:ncol(inpdata)) {
        # i <- 4
        if(sum(is.na(inpdata[i])) == nrow(inpdata)){
          next
          }
        inpdata_reorg[[i]] <- inpdata[i]
        }
      inpdata_reorg <- inpdata_reorg %>% bind_cols()

      inpdata_reorg <-
        foreach(i = 1:(ncol(inpdata_reorg)/3), .combine = 'rbind') %do% {
          ii = 1 + (i - 1)*3
          iii = ii + 2
          inpdata_reorg[,ii:iii] %>% drop_na() %>%
            set_names(., nm = c("score", "freq","GCA"))
        }
      return(inpdata_reorg)
    }
  }
#
freq_to_vec = function(data) {

  rep(data$score, data$freq)
}
#
upper_remove_blank <- function(vec){
  toupper(remove_blank_vector(vec))
    }
#
get_lvnm <-
  function(inpData, GCAID){
    # inpData <- setup_data[, lv_p]; GCAID<- id_list$GCA
    .get_lvnm <-
      function(inpData) {
        # inpData <- first_data[,5]
        lvnm <-
          inpData %>%
          str_split(., ",") %>%
          unlist() %>%
          stri_replace_all_charclass(.,
                                     "\\p{WHITE_SPACE}", "")
        return(lvnm)
      }

    apply(inpData, 1, .get_lvnm) %>%
      data.frame() %>%
      set_names(., nm = GCAID)
  }
#
get_ID <- function(filteredData) {

  GCAID <-
    filteredData %>%
    distinct(GCA) %>%
    pull()

  TableID<-
    filteredData %>%
    distinct(GCA, Table)

  Table_n <- TableID %>% group_by(GCA) %>% count() %>% pull(n)

  p1 <- as.name(names(filteredData)[str_detect(names(filteredData), "Panel|User")])

  UserID <-
    filteredData %>%
    distinct(GCA, Table, !!p1)

  return(list(GCA = GCAID, Table = TableID, Table_n = Table_n,
              PanelID = UserID))
}
#
get_location <- function(fourthData, locNm, testinp){
  # fourthData <- fourth_data; locNm <- input$loc
  dataUsed <- fourthData %>% filter(GCA %in% testinp)
  GCAId <- fourthData %>% filter(GCA %in% testinp) %>% pull(GCA) %>% unique()

  location <-
    dataUsed %>%
    group_split(GCA) %>%
    map(., ~ .x %>% select(GCA, Item_ID, all_of(locNm))) %>%
    set_names(., nm = GCAId)

  bind_loc <-
    location %>%
    map(., ~ .x %>% arrange(!!as.name(locNm)) %>%
          mutate(OOD = 1:nrow(.)) %>%
          select(-c(1:2))
    )
  return(list(location = location, bind_loc = bind_loc))

}
#
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
#
remove_blank_vector <- function(inpData) {

  inpData %>% stri_replace_all_charclass(., "\\p{WHITE_SPACE}", "")
}
#
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
#
estCutScore <- function(inpData, information) {
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
}
#
tab1_group_out <-
  function(inpData, selectedCp, WESS, modal = F){

    # inpData = tab0$est_cs[[1]];selectedCp = tab0$selected_CP[[1]];WESS = input$WESS; modal = F
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
#
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
#
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
gen_ifelse <-
  function(x, lvNames) {
    colors <- c("#FBEEE6","#ffc0cb","#c9ede7","#e3dcf1","#c0ffee","#FBEEE6","#F5B7B1","#D2B4DE","#AED6F1","#A3E4D7","#F9E79F")
    colors <- colors[1:length(lvNames)]

    for(i in 1:length(lvNames)) {
      lvNames[i] <- glue::glue({ "'{lvNames[i]}'"  })
    }

    for(i in 1:length(colors)) {
      colors[i] <- glue::glue({ "'{colors[i]}'"  })
    }

    if_list <- list()
    for(i in 1:length(lvNames)){
      # i <- 1
      lvNames_1 <- lvNames[i]
      colors_1 <- colors[i]

      if(i < length(lvNames)){
        if_list[[i]] <-
          glue::glue({
            "ifelse({x} == {lvNames_1}, {colors_1},"
          })
      } else {
        p1 <- paste( rep(")", (length(lvNames)-1)), collapse = " ")
        if_list[[i]] <- glue::glue({ "{colors_1} {p1}" })
      }
    }
    return(paste(unlist(if_list), collapse = " "))
  }
#
tab2_plot <- function(dataUse, WESS){

  breaks = function(x, xby) x[seq(1, length(x), by=xby)]

  cutLevel <- dataUse %>% pull(`Cut Level`) %>% unique()

  incon_value <- foreach(i = 1:length(cutLevel), .combine = 'cbind') %do% {
    dataUse %>% filter(`Cut Level` == cutLevel[i]) %>% pull(Inconsistency)
  }

  cross_value <- foreach(i = 1:(dim(incon_value)[2] - 1), .combine = 'c') %do% {
    ii = i + 1
    incon_value[which( incon_value[,i] == incon_value[,ii] )[1], 1]
  }

  bk_x = ceiling(length(unique(dataUse$OOD_1))/20)
  x_disc <- breaks(unique(dataUse$OOD_1), bk_x)

  unique_incons <- unique(dataUse$Inconsistency)
  bk_y = ceiling(length(unique_incons)/20)
  y_disc <- breaks(unique(dataUse$Inconsistency), bk_y)

  pp1 <-
    dataUse %>%
    ggplot(aes(x = OOD_1, y = Inconsistency)) +
    geom_point(aes(colour = factor(`Cut Level`)),
               fill = "white", size = 1, stroke = .5, shape = 21) +
    geom_line(aes(group = factor(`Cut Level`),
                  colour = factor(`Cut Level`),
                  linetype = factor(`Cut Level`)),
              size = 1.5,
              alpha = .7) +
    theme_bw(base_size = 16) +
    guides(linetype = FALSE) +
    scale_x_discrete(breaks = x_disc) +
    scale_y_continuous(breaks = y_disc) +
    theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
    theme(legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.position = "bottom",# c(0.025,0.975),
          legend.justification = c(0, 1)) +
    labs(x = "Order of Difficulty",
         y = "Number of Inconsistent Items",
         colour = "Cut Level",
         title = "Number of Inconsistent Items by OOD",
         caption = "")

  if(WESS){
    pp1 <-
      pp1 +
      labs(y = "Total Weights",
           title = "Total Weight by OOD") +
      scale_y_continuous(
        breaks = seq(from = round(min(dataUse$Inconsistency),0), to = round(max(dataUse$Inconsistency),0), length.out = 5)
      )
  }
  pp1
}

lv_por <-
  function(scores, locnum, minp){
    lv_first_por <-
      round(sum(scores < locnum[(minp[1])]) / length(scores), 3)*100

    lv_md_por <-
      foreach(i = 1:(length(minp)-1), .combine = 'c') %do% {

        myi <- i
        round(sum( scores < locnum[(minp[(myi+1)])] & scores >= locnum[(minp[myi])]) / length(scores), 3)*100

      }
    lv_last_por <- round(sum( scores >= locnum[(minp[length(minp)])]) / length(scores), 3)*100

    atabv <-
      foreach(i = 1:(length(minp)), .combine = 'c') %do% {
        round(sum( scores >= locnum[(minp[i])]) / length(scores), 3)*100
      }

    bel <-
      foreach(i = 1:(length(minp)), .combine = 'c') %do% {
        round(sum( scores < locnum[(minp[i])]) / length(scores), 3)*100
      }

    list(pors = c(lv_first_por, lv_md_por, lv_last_por),
         atabv = atabv, bel = bel)
  }
#
gen_page_data <- function(ep, datainp, fourthData, fifthData,
                          pageName, targetLoc, lvnm){
  # pageName <- page_name; targetLoc <- target_loc

  dataUse_1 <- datainp[ep, ]
  gca_id <- dataUse_1 %>% pull(1)
  cutpoint_inp <- dataUse_1 %>% select(all_of(pageName))
  loc_num <- fourthData[[ep]] %>% pull(targetLoc)

  # sclScore <- fifthData[[ep]] %>% pull(2)

  sclScore <-
  if(sum(str_detect(toupper(names(fifthData[[ep]])), toupper("freq"))) == 0) {
    fifthData[[ep]] %>% pull(2)

  } else {
    freq_to_vec(fifthData[[ep]])
  }

  min_point <- cutpoint_inp %>% unlist()

  lv_data <- lv_por(sclScore, loc_num, min_point)

  scale_score <- map(1:length(min_point), ~ loc_num[min_point[.x]]) %>% unlist()

  perc_in <- lv_data$pors
  perc_atabo <- lv_data$atabv
  perc_bel <- lv_data$bel

  scale_scores <-
    scale_score %>%
    tibble( scaleScore = ., Level = lvnm[-1], GCA = gca_id)

  perc_ins <-
    perc_in %>%
    tibble( percIn = ., Level = lvnm, GCA = gca_id)

  perc_atabos <-
    perc_atabo %>%
    tibble( percAtabo = ., Level = lvnm[-1], GCA = gca_id)

  perc_bel <-
    perc_bel %>%
    tibble( percBel = ., Level = lvnm[-1], GCA = gca_id)

  return(
    list(scale_scores = scale_scores,perc_ins = perc_ins,
      perc_atabos = perc_atabos, perc_bel = perc_bel))
  }
#
gen_blank_page <-
  function(GCA, lvnm){
    # GCA = gca_nm; lvnm = information$data_ready$level_nm[[1]]
    `.` <- c("OIB Page","Cut Scores","Percent in Level",
             "Percent at or Above Level","Percent Below Level")
    page_name <- paste0("Level", 1:length(lvnm))
    sum_table_1 <- tibble(GCA, `.`)
    for(pn in page_name) {
      sum_table_1 <- sum_table_1 %>%
        mutate(!!pn := 0)
    }
    return(sum_table_1)
  }
#
summarize_page <-
  function(ep, blankPage, perIns, percAtabove, perBelow, minData, lvnm,
           numItem, locNum){
    # ep=1; blankPage = blank_page; perIns = perIn; percAtabove = percAtabv;
    # perBelow = percBel; minData = min_data; lvnm = level_names; numItem = num_item; locNum <- loc_num

    sum_page <- blankPage %>% .[[ep]]
    perIn_c <- perIns %>% .[[ep]] %>% pull(perIn_c)
    percAtabo_c <- percAtabove %>% .[[ep]] %>% pull(percAtabo_c)
    percBel_c <- perBelow %>% .[[ep]] %>% pull(percBel_c)
    min_point <- minData[ep, ] %>% unlist()
    numItem_1 <- numItem %>% .[[ep]]
    locNum_1 <- locNum %>% .[[ep]] %>% unlist()

    levelData <- vector("list", length(lvnm))
    levelData[[1]] <- c(glue("1-{min_point[1]-1}"), "", perIn_c[1], "100%", "" )

    for(i in 1:length(min_point)) { # i = 1
      ii <- i + 1
      point_inp <- ifelse(ii <= length(min_point), min_point[ii] - 1, numItem_1)
      levelData[[ii]] <- c(glue("{min_point[i]}-{point_inp}"), locNum_1[min_point[i]],perIn_c[ii], percAtabo_c[i], percBel_c[i])
    }

    for(li in 1:length(levelData)) { # li = 1
      lii <- li + which(names(sum_page)==".")
      sum_page[,lii] <- levelData[[li]]
    }
    return(sum_page)
  }
#
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
#
dt_table_out_indi <- function(tab1_indi_res, table_options){
  # tab1_indi_res <- tab1$indi_table
  maxRow <- tab1_indi_res %>% count(GCA) %>% pull(2) %>% .[1]
  grades <- tab1_indi_res %>% pull(1) %>% unique()
  colors <- c('#DAF7A6',"#A6B1F7","#A6F7C3","#A6DAF7","#FFC300")

  tab1_indi_res$Correlation <- round(tab1_indi_res$Correlation,2)

  level_name <- names(tab1_indi_res)[str_detect(names(tab1_indi_res), "_p")]
  level_name <-
    str_split(level_name, "_p") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")
  num_level <- length(level_name)

  levels <-
    foreach(pi = 1:num_level, .combine = 'c') %do% {
      glue::glue("th(colspan = 2, '{level_name[pi]}'),")

    } %>% paste(., collapse = "\n")
  con_dt <- glue::glue(
    "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),
      th(rowspan = 2, 'Panelist'),
      th(rowspan = 2, 'Correlation'),

      th(colspan = {num_level}, 'Pages'),

      {levels}

      th(colspan = 2, 'SUM'),

    tr(
      lapply(c(level_name,rep(c('Count','Weight'), (num_level+1))), th)
      )

      )
    )
  )
)"
  )

  DT::datatable(tab1_indi_res,
                container = eval(parse(text = con_dt)),
                class = 'table-bordered stripe table-condensed',
                # filter = 'top',
                rownames = F,
                extensions =
                  c('RowGroup'),
                options = match.fun(table_options)(maxRow)


  ) %>%
    formatStyle(1,
                backgroundColor = styleEqual(grades,
                                             colors[1:length(grades)]
                )
      )

}
#
dt_table_out_mode <- function(tab1_indi_res, table_options){

  maxRow <- nrow(tab1_indi_res)
  grades <- tab1_indi_res %>% pull(1) %>% unique()
  colors <- c('#DAF7A6',"#A6B1F7","#A6F7C3","#A6DAF7","#FFC300")

  tab1_indi_res$Correlation <- round(tab1_indi_res$Correlation,2)

  level_name <- names(tab1_indi_res)[str_detect(names(tab1_indi_res), "_p")]
  level_name <-
    str_split(level_name, "_p") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")
  num_level <- length(level_name)

  levels <-
    foreach(pi = 1:num_level, .combine = 'c') %do% {
      glue::glue("th(colspan = 2, '{level_name[pi]}'),")

    } %>% paste(., collapse = "\n")
  con_dt <- glue::glue(
    "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),
      th(rowspan = 2, 'Correlation'),

      th(colspan = {num_level}, 'Pages'),

      {levels}

      th(colspan = 2, 'SUM'),

    tr(
      lapply(c(level_name,rep(c('Count','Weight'), (num_level+1))), th)
      )

      )
    )
  )
)"
  )

  DT::datatable(tab1_indi_res,
                container = eval(parse(text = con_dt)),
                class = 'table-bordered stripe table-condensed',
                # filter = 'top',
                rownames = F,
                extensions =
                  c('RowGroup'),
                options = match.fun(table_options)(maxRow)

  ) %>%
    formatStyle(1,
                backgroundColor = styleEqual(grades,
                                             colors[1:length(grades)]
                )
    )
}
#
dt_table_out_med <- function(tab1_indi_res, table_options){

  maxRow <- nrow(tab1_indi_res)
  grades <- tab1_indi_res %>% pull(1) %>% unique()
  colors <- c('#DAF7A6',"#A6B1F7","#A6F7C3","#A6DAF7","#FFC300")

  level_name <- names(tab1_indi_res)[str_detect(names(tab1_indi_res), "_p")]
  level_name <-
    str_split(level_name, "_p") %>%
    map(., ~ .x[[1]]) %>%
    unlist() %>%
    str_replace(., "L", "Level")
  num_level <- length(level_name)

  levels <-
    foreach(pi = 1:num_level, .combine = 'c') %do% {
      glue::glue("th(colspan = 2, '{level_name[pi]}'),")

    } %>% paste(., collapse = "\n")
  con_dt <- glue::glue(
    "container_dt = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'GCA'),
      th(rowspan = 2, 'Table'),

      th(colspan = {num_level}, 'Pages'),


    tr(
      lapply(c(level_name), th)
      )

      )
    )
  )
)"
  )

  DT::datatable(tab1_indi_res,
                container = eval(parse(text = con_dt)),
                class = 'table-bordered stripe table-condensed',
                # filter = 'top',
                rownames = F,
                extensions =
                  c('RowGroup'),
                options = match.fun(table_options)(maxRow)

  ) %>%
    formatStyle(1,
                backgroundColor = styleEqual(grades,
                                             colors[1:length(grades)]
                )
    )
}
#
tab2_table_effpage <-
  function(dataInp) {

    dataUse_1 <- dataInp

    ln <- names(dataUse_1)[str_detect(names(dataUse_1), "_W")]
    ln <-
      str_split(ln, "_W") %>%
      map(., ~ .x[[1]]) %>%
      unlist() %>%
      str_replace(., "L", "Level")

    level_break <-
      foreach(lll = 1:length(ln), .combine = 'c') %do% {
        a1 <- ln[lll]
        glue::glue("'{a1}' = 2")
      } %>% paste(., collapse = ", ")


    level_break <- glue::glue('c(" " = 3, {level_break},"SUM" = 2)')

    dataUse_1 %>%
      knitr::kable(format = "html", escape = F,
                   align = 'c') %>%
      kable_styling(
        c("striped","condensed"),
        full_width = F,
        font_size = 13
      ) %>%

      row_spec(0, bold = T) %>%

      add_header_above(
        eval(parse(text =level_break))
      )
  }
#
tab2_table <-
  function(dataInp, WESS = information$base_data$WESS_nm) {

    dataUse_1 <- dataInp

    ln_0 <- names(dataUse_1)[(which(names(dataUse_1) == "ALD")+1):(ncol(dataUse_1)-1)]
    ln <- sort(ln_0)

    new_order_name <- c( names(dataUse_1)[1:which(names(dataUse_1) == "ALD")], ln,
                         names(dataUse_1)[ncol(dataUse_1)])

    dataUse_1 <- dataUse_1 %>% select(all_of(new_order_name))

    coloring <- function(x) {
      cell_spec(x, "html", background = ifelse(x == min(x),
                                               "#00FFFD", "transparent"))
    }

    oplv <- dataUse_1 %>% pull(Operational_Lv)
    oplv_names <- oplv %>% unique()

    kable.line <- c()
    for(oi in 1:length(oplv_names)){
      o_p <- which(oplv == oplv_names[oi])
      kable.line[oi] <- o_p[length(o_p)]
    }

    unselect <- (which(names(dataUse_1) == "GCA")+1):(which(names(dataUse_1) == "Item_ID")-1)

    selected <- names(dataUse_1)[-unselect]

    dataUse_1 <-
      dataUse_1 %>%
      select(all_of(selected))

    if(WESS){
      ln_1 <- ln_0[str_detect(ln_0, "_W")]
    } else {
      ln_1 <- ln_0[!str_detect(ln_0, "_W")]
    }
    shading_cols <- which(names(dataUse_1) %in% ln_1)

    lvs <- ln_0[!str_detect(ln_0, "_W")]

    level_break <-
      foreach(lll = 1:length(lvs), .combine = 'c') %do% {
        a1 <- lvs[lll]
        glue::glue("'{a1}' = 2")
      } %>% paste(., collapse = ", ")


    level_break <- glue::glue('c(" " = 5, {level_break}," " = 1)')

    aa <- ncol(dataUse_1)

    dataUse_1 %>%
      mutate_at(ln, list(coloring)) %>%
      mutate(Operational_Lv =
               cell_spec(Operational_Lv,
                         background =
                           eval(parse(text = gen_ifelse("Operational_Lv",oplv_names)
                           )
                           )
               )
      ) %>%
      knitr::kable(format = "html", escape = F,
                   align = 'c') %>%
      kable_styling(
        c("striped","condensed"),
        full_width = F,
        font_size = 13
      ) %>%
      column_spec(aa, bold = T, border_left = T) %>%
      column_spec(shading_cols, background = "#D8E0DF") %>%
      # column_spec(1:ncol(dataUse_1), align = "center") %>%
      row_spec(0, bold = T) %>%
      collapse_rows(., columns = 1:2, valign = "top") %>%

      row_spec(., kable.line, extra_css = "border-bottom: 1px solid") %>%
      add_header_above(
        eval(parse(text =level_break))
      )
  }
#
tab2_plot_data <- function(dataInp, information, vvv){

  dataUse_1 <- dataInp
  # SD <- ifelse(is.na(information$data_ready$SD_data[vvv]), 1,
  #              information$data_ready$SD_data[vvv])

  target_filter <- information$base_data$target_nm
  targ_p <- which(names(dataUse_1) == target_filter)
  lv_names <- names(dataUse_1)[(targ_p + 1):(ncol(dataUse_1)-1)]
  loc_nm <- information$base_data$loc_nm

  dataUse_1 <-
    dataUse_1 %>%
    mutate(OOD_1 = paste0(OOD,"\n", !!as.name(loc_nm)),
           OOD_1 = factor(OOD_1, levels = paste0(OOD,"\n", !!as.name(loc_nm))
           )
    ) %>%

    select(OOD_1, !!as.name(loc_nm), (targ_p + 1):(ncol(dataUse_1)-1)) %>%
    gather(., "Cut Level", "Inconsistency", -c(OOD_1), -!!as.name(loc_nm))

  if(information$base_data$WESS_nm) {
    dataUse_1 <-
      dataUse_1 %>%
      filter( str_detect(`Cut Level`, "_W" ))
    # %>%
    #   mutate(Inconsistency = round(Inconsistency/SD,1))

  } else {
    dataUse_1 <-
      dataUse_1 %>%
      filter( !str_detect(`Cut Level`, "_W" ))
  }
  return(dataUse_1)
}

#------------------------------------------------------------------
# Word Report
#------------------------------------------------------------------
# Specify 'Class'
classAppend <- function(x, classname){
  class(x) <- append(class(x), classname)
  return(x)
}
# Generic for Word Tables
mytable <- function(table_inp, ...){

  UseMethod("mytable", table_inp)
}
#
mytable.inpdata <- function(x, caption = "Table: Example") {

  flextable(x) %>%
    set_caption(
      caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%
    border(.,
           border.top = fp_border(color = "black", width = 1.25),
           border.bottom = fp_border(color = "black", width = .75),
           part = "header"# partname of the table (all body header footer)
           ) %>%
    border_inner_h(.,
                   border = fp_border(color="transparent", width = 1)) %>%
    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.25) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = .9, layout = "autofit")

}
#
mytable.ESSresult1 <- function(table_inp, caption = "Table. Example") {

  # table_inp <- modal_table
  table_inp$Cors <- round(table_inp$Cors, 3)

  line_n <- table_inp %>% count(GCA) %>% pull(n) %>% cumsum()
  col_names <- names(table_inp)
  cors_p = which(col_names=="Cors")
  non_multi = 1:which(col_names=="Cors")
  pages = which(str_detect(col_names, "_p"))

  level_names_0 <- col_names[-c(non_multi, pages)]
  num_set <- length(level_names_0)/2

  count_weight <- c()
    for(i in 1:num_set-1){
    count_weight[i] <- pages[length(pages)] + 2*(i)
    }

  level_names_each <- rep(paste("Level", 2:(num_set)), each = 2)
  level_names_sum <-  rep("SUM", each = 2)

  # Header 1
  header1 <- c()
  header1[non_multi] <- " "
  header1[pages] <- "Pages"
  header1[(pages[length(pages)]+1):(length(col_names)-2)] <- level_names_each
  header1[(length(col_names)-1):length(col_names)] <- level_names_sum
  # Header 2
  header2 <- c()
  header2[non_multi] <- col_names[non_multi]
  header2[pages] <- paste("Level", 1:length(pages))
  header2[(pages[length(pages)]+1):length(col_names)] <-
    rep(c("Ct","Wt"), (num_set))


  multiple_header <- data.frame(
    col_keys = col_names,
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- flextable( table_inp )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, part = "header")

  ft_1 %>%
  set_caption(caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%

    border(.,
           i = 1, # row selection
           border.top = fp_border(color = "black", width = 1.25),
           border.bottom = fp_border(color = "black", width = .75),
           part = "header"# partname of the table (all body header footer)
           ) %>%

    border(.,
           i = 2, # row selection
           border.bottom = fp_border(color = "black", width = 1.25),
           part = "header"# partname of the table (all body header footer)
           ) %>%
    border(.,

           j = c(cors_p, pages[c(length(pages))]), # column selection)
           border.right = fp_border(color = "black", width = .75),
           part = "all"# partname of the table (all body header footer)
           ) %>%

    border(.,
      j = count_weight, # column selection)
      border.right = fp_border(color = "black", width = .75),
      part = "all"# partname of the table (all body header footer)
      ) %>%

    border(.,
           i = line_n, # row selection
           border.bottom = fp_border(color = "black", width = 0.75),
           part = "body"# partname of the table (all body header footer)
           ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.25) ) %>%
    bold(i = c(1,2), part = 'header') %>%

    merge_v(j = c(1,2), part = "body") %>%
    valign(j = c(1,2), valign = 'top') %>%
    set_table_properties(width = 1, layout = "autofit")
}
#
mytable.ESSresult2 <- function(table_inp, caption = "Table. Example") {
  # table_inp <- tab1$median_table
  line_n <- table_inp %>% count(GCA) %>% pull(n) %>% cumsum()

  col_names <- names(table_inp)

  non_multi = 1:which(col_names=="Table")
  pages = which(str_detect(col_names, "_p"))

  # Header 1
  header1 <- c()
  header1[non_multi] <- " "
  header1[pages] <- "Pages"

  # Header 2
  header2 <- c()
  header2[non_multi] <- col_names[non_multi]
  header2[pages] <- paste("Level", 1:length(pages))

  multiple_header <- data.frame(
    col_keys = col_names,
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- flextable( table_inp )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )

  ft_1 <- merge_h(ft_1, part = "header")


  ft_1 %>%
  set_caption(caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%

    merge_v(j = c(1), part = "body") %>%
    valign(j = c(1), valign = 'top') %>%

    border(.,
           border.top = fp_border(color = "black", width = 1.25),
           part = "header"# partname of the table (all body header footer)
           ) %>%
    border(.,
           i = 2, # row selection

           border.top = fp_border(color = "black", width = .75),
           border.bottom = fp_border(color = "black", width = 1.25),
           part = "header"# partname of the table (all body header footer)
           ) %>%

    border(.,
           i = line_n, # row selection

           border.bottom = fp_border(color = "black", width = 0.75),
           part = "body"# partname of the table (all body header footer)
           ) %>%
    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.25) ) %>%

    bold(i = c(1,2), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = .8, layout = "autofit")
}
#
mytable.detailESS <- function(table_inp, caption = "Table. Example") {

  # table_inp <- tab2$for_tab2_out[[1]][[1]][["t_out"]]
  table_inp$Round <- NULL
  names(table_inp)[c(2,4)] <- c("ID", "LOC")
  col_names <- names(table_inp)

  non_multi = 1:which(col_names=="ALD")
  weights = which(str_detect(col_names, "_W"))
  counts = which(col_names %in% col_names[-c(non_multi, weights, length(col_names))])
  new_order <- col_names[c(counts, weights)] %>% sort()

  class(table_inp) <- "data.frame"

  table_inp <- table_inp %>% select(non_multi, new_order, everything())
  col_names <- names(table_inp)
  #
  level_names_0 <- col_names[counts]
  num_set <- length(counts)
  #
  level_names_each <- rep(paste("Level", 2:(num_set+1)), each = 2)

  # Header 1
  header1 <- c()
  header1[non_multi] <- " "
  header1[c(counts, weights)] <- level_names_each
  header1[length(col_names)] <- " "

  # Header 2
  header2 <- c()
  header2[non_multi] <- col_names[non_multi]
  header2[c(counts, weights)] <- rep(c("Ct","W"), num_set)
  header2[length(col_names)] <- "OpLV"

  multiple_header <- data.frame(
    col_keys = col_names,
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

    weights = which(str_detect(col_names, "_W"))
  counts = which(col_names %in% col_names[-c(non_multi, weights, length(col_names))])

  ft_1 <- flextable( table_inp )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )

  ft_1 <- merge_h(ft_1, part = "header")


  ft_1 %>%
  set_caption(caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%

    merge_v(j = c(1,2), part = "body") %>%
    valign(j = c(1,2), valign = 'top') %>%

    border(.,
           border.top = fp_border(color = "black", width = 1.25),
           part = "header"# partname of the table (all body header footer)
           ) %>%
    border(.,
           i = 2, # row selection
           border.top = fp_border(color = "black", width = 0.75),
           border.bottom = fp_border(color = "black", width = 1.25),
           part = "header"# partname of the table (all body header footer)
           ) %>%

    border(.,

           j = counts, # column selection)
           border.left = fp_border(color = "black", width = .75),
           part = "all"
           ) %>%
        border(.,

           j = weights[length(weights)], # column selection)
           border.right = fp_border(color = "black", width = .75),
           part = "all"
           ) %>%
    border_inner_h(.,
                   border = fp_border(color="transparent", width = 1)) %>%
    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 2) ) %>%
    padding(.,
            padding = 0.5,
            part = "all") %>%

    bold(i = c(1,2), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = 1, layout = "autofit")
}
#
mytable.crosst <- function(table_inp, caption = "Table. Example") {

  # table_inp <- for_tab2_out[[vi]][[1]][["crosst"]]
  table_inp <-
    as.data.frame.matrix(table_inp) %>%
    mutate(".." := rownames(.),
      .before = 1) %>%
    mutate("." := "Operational Level",
      .before = 1)

  col_names <- names(table_inp)

  # Header 1
  header1 <- c()
  header1[1:2] <- " "
  header1[3:length(col_names)] <- "Aligned ALD"

  # Header 2
  header2 <- c()
  header2[1:2] <- ""
  header2[3:length(col_names)] <- col_names[-c(1:2)]

  multiple_header <- data.frame(
    col_keys = col_names,
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- flextable( table_inp )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, part = "header")


  ft_1 %>%
  set_caption(caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%

    merge_v(j = c(1), part = "body") %>%
    valign(j = c(1), valign = 'top') %>%

    border(.,

           border.top = fp_border(color = "black", width = 1.25),
           border.bottom = fp_border(color = "white", width = 1),
           part = "header"# partname of the table (all body header footer)
           ) %>%
    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.25) ) %>%
    bold(
      i = c(1),

      part = 'all') %>%
    bold(

      j = c(1),
      part = 'all') %>%

    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = .8, layout = "autofit")
}
#
mytable.effpage <- function(table_inp, caption = "Table. Example") {

  # table_inp <- tab3$eff_page
  # caption = "a"
  line_n <- table_inp %>% count(GCA) %>% pull(n)
  line_n <- cumsum(line_n)

  ft_1 <- flextable( table_inp )

  ft_1 %>%
  set_caption(caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%

    merge_v(j = c(1), part = "body") %>%
    valign(j = c(1), valign = 'top') %>%

    border(.,

           border.top = fp_border(color = "black", width = 1.25),
           border.bottom = fp_border(color = "black", width = 0.75),

           part = "header"# partname of the table (all body header footer)
           ) %>%
    border(.,
           i = line_n,
           # j = , # column selection)
           border.bottom = fp_border(color = "black", width = .75),
           part = "body"
           ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.25) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    align(j = 2, align = 'left', part = "body") %>%
    set_table_properties(width = 1, layout = "autofit")
}
#
mytable.review <- function(table_inp, caption = "Table. Example") {

  # table_inp <- tab4$for_tab4_out
  names(table_inp) <- c("GCA","ID", "OOD", "Aligned ALD",
    "Op Level", "Diff Lv", "LOC", "Diff:Loc-Cut", "Cut", "Std Weight")
  ft_1 <- flextable( table_inp )

  ft_1 %>%
  set_caption(caption = caption) %>%
    font(fontname = "Times", part = "all") %>%
    fontsize(size = 12, part = "header") %>%
    fontsize(size = 12, part = "body") %>%

    border(.,

           border.top = fp_border(color = "black", width = 1.25),
           border.bottom = fp_border(color = "black", width = .75),

           part = "header"# partname of the table (all body header footer)
           ) %>%

    border_inner_h(.,
                   border = fp_border(color="transparent", width = .75)) %>%
    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.25) ) %>%
    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = 1, layout = "autofit")
}
#
text_add <-
  function(x.doc, x.text, x.align = "left", x.fsize = 12, x.bold = F){

    body_add_fpar(
    x.doc,
    fpar(
      ftext(
        text = x.text,
        prop =
          fp_text(
            font.size = x.fsize, bold = x.bold,
            font.family = "Times")),
      fp_p = fp_par(text.align = x.align)
      )
  )
}
#
table_add <- function(x.doc, x.tb) {
  flextable::body_add_flextable(
    x.doc,
    value = x.tb,
    align = "left"
    )

  body_add_par(x.doc, " ")
}
#
plot_add <- function(x.doc, gg, p.title) {
  body_add_gg(x.doc,gg,
    width = 7, height = 5) %>%
  body_add_par(value = p.title, style = "Image Caption")

  body_add_par(x.doc, " ")
}
# Generate Word Documnet
word_out3 <-
  function(
    # filename = file_docx;reportTables = reportTables;
    #     for_tab2_out = for_tab2_out; tab3 = tab3
    filename,

    titleInp = "Embedded Standard Setting Technical Report",
    stateNM = "State Name",
    testNM = "Testing Program Name",
    admNM = "Creative Measurement Solutions LLC",
    reportTables, for_tab2_out, tab3){

  todaydate<- as.character(Sys.Date())
  fakewords = "Input Text"

  my.doc =
    tryCatch(
      read_docx(path = "www/template.docx"),
      error = function(cond) {
        read_docx()
      }
    )

  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")

  text_add(my.doc, x.text = titleInp, x.align = "center", 16, F)
  body_add_par(my.doc, " ")

  text_add(my.doc, x.text = stateNM, x.align = "center", 14, F)
  text_add(my.doc, x.text = testNM, x.align = "center", 14, F)

  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")
  body_add_par(my.doc, " ")

  text_add(my.doc, x.text = admNM, x.align = "center", 14, F)
  text_add(my.doc, x.text = todaydate, x.align = "center", 14, F)

  body_add_break(my.doc, pos = "after")

  body_add_par(my.doc, "Table of Contents", style = "Normal")
  body_add_toc(my.doc, level = 2)
  body_add_break(my.doc)

  body_add_par(my.doc, value = "Introduction", style = "heading 1")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_par(my.doc, value = "Grades, Content Areas, and Panelists",
      style = "heading 2")

  table_add(my.doc, reportTables[["setup"]])

  table_add(my.doc, reportTables[["panel"]])

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_break(my.doc, pos = "after")

  body_add_par(my.doc, value = "Data",
      style = "heading 1")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_break(my.doc, pos = "after")

  body_add_par(my.doc, value = "Method",
      style = "heading 1")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_break(my.doc, pos = "after")

  body_add_par(my.doc, value = "Results",
      style = "heading 1")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_par(my.doc, value = "ESS Individual Results",
      style = "heading 2")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  my.doc = body_end_section_continuous(my.doc)

  table_add(my.doc, reportTables[["indi"]])

  my.doc = body_end_section_landscape(my.doc)

  body_add_par(my.doc, value = "ESS Group Modal Results",
      style = "heading 2")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  my.doc = body_end_section_continuous(my.doc)

  table_add(my.doc, reportTables[["modal"]])

  my.doc = body_end_section_landscape(my.doc)

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_par(my.doc, value = "Cross Tabs",
      style = "heading 2")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  cross_name <- names(reportTables)[str_detect(names(reportTables), "crosst")]
  for(cti in 1:length(cross_name)){
    # i <- 1
    table_add(my.doc,reportTables[[cross_name[cti]]])
    }

  body_add_par(my.doc, value = "ESS Group Median Results",
      style = "heading 2")

  table_add(my.doc, reportTables[["med"]])

  body_add_par(my.doc, value = "Detailed ESS Group Results",
      style = "heading 2")

  text_add(my.doc, x.text = "", x.align = "left", 12, F)

  detail_name <- names(reportTables)[str_detect(names(reportTables), "detailESS")]
  for(di in 1:length(detail_name)){
    table_add(my.doc,reportTables[[detail_name[di]]])

    plot_add(my.doc, for_tab2_out[[di]][[1]][["p1"]], paste0("Figure ",di,". "))
  }

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  body_add_par(my.doc, value = "Cut Score and Impact Data",
      style = "heading 2")

  text_add(my.doc, x.text = "", x.align = "left", 12, F)

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  table_add(my.doc, reportTables[["effpage"]])

  page_plot <- names(tab3)[str_detect(names(tab3), "^p_")]
  for(i in 1:length(page_plot)){
    plot_add(my.doc, tab3[[ page_plot[i] ]], paste0("Figure 8-",i,". "))
  }

  body_add_par(my.doc, value = "Item Review: Lists of Inconsistency Items by Grade",
      style = "heading 2")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  table_add(my.doc, reportTables[["ireview"]])

  body_add_break(my.doc, pos = "after")

  body_add_par(my.doc, value = "Discussion and Actionable Recommendations",
      style = "heading 1")

  text_add(my.doc, x.text = fakewords, x.align = "left", 12, F)

  print(my.doc, target = filename)
  }

#----------------------------------------------------------
# table options
#----------------------------------------------------------
table_options_button <- list(
            dom = 't',
            #Bfrtip
            pageLength = 10,
            buttons = list(
              c('copy', 'csv', 'excel', 'pdf', 'print'),
              list(
                extend = "collection",
                text = 'Show All',
                action =
                  DT::JS(
                  "function ( e, dt, node, config ) {dt.page.len(-1);
                    dt.ajax.reload();}"
                    )
                )
            ),
            deferRender = TRUE,
            lengthMenu = list(c(10, 20,-1), c('10', '20', 'All')),
            searching = FALSE,
            editable = TRUE,
            scroller = TRUE,
            lengthChange = FALSE,
            autoWidth = TRUE,
            initComplete =
            JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000','text-align': 'center'});",
              "}"
              ),
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          )

table_options <- list(
  dom = 't',
  #Bfrtip
  pageLength = 10,
  deferRender = TRUE,
  lengthMenu = list(c(10, 20,-1), c('10', '20', 'All')),
  searching = FALSE,
  editable = TRUE,
  scroller = TRUE,
  scrollX = T,
  fixedHeader = TRUE,
  lengthChange = TRUE,
  autoWidth = T,
  rowGroup = list(1),
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000', 'text-align': 'center'});",
    "}"
  ),
  columnDefs =
    list(
      list(className = 'dt-center', targets = "_all")
    )
)

table_options2 <-
  list(
    dom = 't',
    #Bfrtip
    pageLength = 12,
    deferRender = TRUE,
    lengthMenu = list(c(10, 20,-1), c('10', '20', 'All')),
    fixedHeader = TRUE,
    lengthChange = FALSE,
    autoWidth = F,
    scrollX = T,
    # rowGroup = list(dataSrc = 1),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000', 'text-align': 'center'});",
      "}"
    ),
    columnDefs = list(
      list(
        className = 'dt-center', targets = "_all"
      )
    )
  )

table_options_new_1 <- function(maxRow){
  list(
    dom = 'Bftrip',
    pageLength = maxRow,
    scrollX = T,
    scroller = TRUE,
    fixedHeader = TRUE,
    autoWidth = F,
    rowGroup = list(dataSrc = c(1)),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '##DEF7F9', 'color': '#000', 'font-weight': 'bold', 'text-align': 'center'});",
      "}"
    ),
    columnDefs = list(
      list(
        className = 'dt-center', targets = "_all"
      )
    )
  )
}

table_options_new_2 <- function(maxRow){
  list(
    dom = 't',
    pageLength = maxRow,
    scrollX = T,
    scroller = TRUE,
    fixedHeader = TRUE,
    autoWidth = F,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '##DEF7F9', 'color': '#000', 'font-weight': 'bold', 'text-align': 'center'});",
      "}"
    ),
    columnDefs = list(
      list(
        className = 'dt-center', targets = "_all"
      )
    )
  )
}

extract_num <- function(vectorInp){
  as.numeric(str_extract(vectorInp, "[[:digit:]]"))
}

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
