rm(list = ls())

root <- rprojroot::find_rstudio_root_file()
data_path <- file.path(root, "test/data")
source_path <- file.path(root, "test/original")

source(file.path(source_path, "global.R"))

input <- list()
# input$tests <- c("M4","R5")
input$tests <- c("M3","M4")
input$targets <- "ALD"#
input$WESS = F
input$threshold = T
input$median = "modal"

path <- fs::dir_ls(data_path)[1]
input$loc <- "Loc_RP67" # input$loc
# input$loc <- "Loc_RP67" # input$loc

imprt_data <-
  fs::dir_ls(data_path) %>%
  .[1] %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)
###############################################################################
###############################################################################
# Replace the below with the credential of yours
# rsconnect::setAccountInfo(name='sooyonglee',
#                           token='1EE8C796F4000020C45FFBF46C91993B',
#                           secret='0GRStcVByTmiboQ+7TbKEvqN7zEvyHq4VV3rFTaB')
# rsconnect::setAccountInfo(name='creativemeasurementsolutionsllc', token='18CBE63023FF268303FD90AB285F187C', secret='49JXMnwDcc3R/MzQQKZfjTchJZiA6aD/6ww9R0o9')
# source("./global.R")
#
# shinyServer(function(input, output, session) {

# Data Import
#--------------------------------------------------------------
  # imprt_data <- reactive({
  #   path <- input$setups$datapath
  #   path %>%
  #     excel_sheets() %>%
  #     set_names() %>%
  #     map(read_excel, path = path)
  #   })
  setup_data    <- dataReady_setup(imprt_data)
  rating_data   <- dataReady_rating(imprt_data)
  item_data     <- dataReady_itemdata(imprt_data)
  examinee_data <- dataReady_examineedata(imprt_data)
#

#   observeEvent(input$import, {
#     output$setting <- renderTable({imprt_data()[[1]]},
#       striped = T,
#       align = "c",
#       width = '70%',
#       caption = "Setup",
#       caption.placement = getOption("xtable.caption.placement", "top")
#       )
#   })
# #
#  observeEvent(input$import, {
#     output$itemtable1 <- renderTable({
#
#
#
#       imprt_data()[[3]] %>%
#         set_names(nm = c("GCA","Subject","Grade","Round","Table","Panelist","Item_ID",	"ALD")) %>%
#         group_by(GCA, Subject, Grade, Round, Panelist) %>%
#         summarise(
#           `Number of Item` = n()
#         )
#       },
#       striped = T,
#       align = "c",
#       width = '50%',
#       caption = "Number of Items on Rating",
#       caption.placement = getOption("xtable.caption.placement", "top")
#       )
#   })
#
#  observeEvent(input$import, {
#     output$itemtable2 <- renderTable({
#
#       imprt_data()[[4]] %>%
#         group_by(GCA) %>%
#         summarise(
#           `Number of Item` = n()
#         )
#       },
#       striped = T,
#       align = "c",
#       width = '30%',
#       caption = "Number of Items on Item Meta Data",
#       caption.placement = getOption("xtable.caption.placement", "top")
#       )
#   })
# Input-Information-Ready
#--------------------------------------------------------------
  # observeEvent(input$import, {
  #   id_selected_choices <- pull_unique(setup_data(), 1)
  #   loc_selected_choices <-
  #     item_data() %>%
  #     select(
  #       which(str_detect(toupper(names(.)), toupper("loc")))
  #       ) %>%
  #   names(.)
  #
  #   loc_selected_choices <-
  #     loc_selected_choices[str_detect(toupper(loc_selected_choices), toupper("Loc"))]
  #
  #   if(sum(str_detect(loc_selected_choices, "67")) != 1){
  #     selected_choices <- loc_selected_choices[1]
  #   } else {
  #     selected_choices <-
  #       loc_selected_choices[str_detect(loc_selected_choices, "67")]
  #   }
  #
  #   updateCheckboxGroupInput(
  #     session,
  #     inputId = "tests",
  #     label = NULL,
  #     choices = id_selected_choices,
  #     selected = id_selected_choices,
  #     inline = T
  #   )
  #
  #   updateRadioButtons(
  #     session,
  #     inputId = "loc",
  #     label = NULL,
  #     choices = loc_selected_choices,
  #     selected = selected_choices,
  #     inline = T
  #   )
  # })

# Data Filtering
#---------------------------------------------------------------------------
  # filtered_data <-
  #   eventReactive(input$tests, {
  #     req(rating_data())
      filtered_data <- data_filter(rating_data, input$tests)
      # return(filtered_data)
    # })

# Information Data Ready
#---------------------------------------------------------------------------
  information <- list() # reactiveValues()
  # observeEvent(input$run_tab1,
  # {
    information$imported_data <-
      list(
        setup_data     = setup_data,
        rating_data    = rating_data,
        item_data      = item_data,
        examinee_data  = examinee_data
      )
    information$base_data <-
      list(
        target_nm = "ALD",
        loc_nm = input$loc,
        WESS_nm = input$WESS,
        modal_nm = "modal",
        threshold = F,
        filtered_data = filtered_data
      )
    id_list = get_ID(filtered_data)

    SD_data =
      tryCatch({
        a1 <- setup_data %>% filter(GCA %in% id_list$GCA) %>% pull(SD)
        a1[which(is.na(a1))] <- 1
        a1
      },
      error = function(e) 1
      )

    SD_data <- data.frame(GCAid = id_list$GCA, SD = SD_data)

    lv_p <-
      which(upper_remove_blank(names(setup_data)) ==
        upper_remove_blank("Level Options"))

    setup_data <- setup_data

    level_nm0 <- setup_data[setup_data$GCA %in% input$tests, lv_p]

    information$data_ready <-
      list(
        id_list = id_list,
        level_nm = get_lvnm(level_nm0, input$tests),
        location_ready = get_location(item_data, input$loc, id_list$GCA),
        SD_data = SD_data
      )
    information$split_data <-
      filtered_data %>%
      group_split(!!as.name(names(filtered_data)[str_detect(names(filtered_data), "Panel|User")]))
  # })

# Calculate Cut Scores
#---------------------------------------------------------------------------
  # waitress <- Waitress$new("#run_tab1", theme = "overlay", infinite = TRUE)

  tab0 <- list() #reactiveValues()
  # observeEvent(input$run_tab1, # Estimate Cut Score and Cut Point
  # {
  #
  #   waitress$start()

      tab0$est_cutscore <-
        map(information$split_data, estCutScore, information) %>%
        set_names(.,
          nm = information$data_ready$id_list[["PanelID"]] %>%
            filter(GCA %in% input$tests) %>%
            pull(3) %>% unique())

    tab0$est_cs <- map(tab0$est_cutscore, ~ .x$est_cs)
    tab0$est_cp <- map(tab0$est_cutscore, ~ .x$est_cp)
    tab0$selected_CP <- map(tab0$est_cutscore, ~ .x$selected_CP)
  # })
#
# Individual Cut Scores
#---------------------------------------------------------------------------
  tab1 <- list() # reactiveValues()
  # observeEvent(input$run_tab1, # Obtain Operational Level for Individuals
  # {
    tab1$res <-
      map2(tab0$est_cs, tab0$selected_CP,
           tab1_group_out, input$WESS, modal = F) %>%
      map(., ~.x$res) %>% bind_rows() %>%
      select(-OOD)
  # })

#
  # observeEvent(input$run_tab1,
  # {
    # Ready for Individual Table for output
    tab1$indi_table <- gen_indi_table(tab1$res)

    # For Median Table ready
    tab1$median_res <- gen_median_table(tab1$res)
    tab1$median_res_all <- gen_median_table_all(tab1$res)
    tab1$median_res_com <-
      bind_rows(tab1$median_res,tab1$median_res_all) %>%
      arrange(GCA)

    # For Median Table output ready
    tab1$median_table <- gen_median_output(tab1$median_res_com)

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

    tab1$modal_res <-
      map2(tab1$modal_est_cs, tab1$modal_selected_cp,
           tab1_group_out, input$WESS, modal = T) %>%
      map(., ~.x$res) %>% bind_rows() %>%
      select(-OOD)

    tab1$modal_res_all <-
      map2(tab1$modal_est_cs_all, tab1$modal_selected_cp_all,
           tab1_group_out_all, input$WESS, modal = T) %>%
      map(., ~.x$res) %>% bind_rows() %>%
      select(-OOD) %>%
      mutate(Table = 0, .after = GCA)

    tab1$modal_res_com <-
      bind_rows(tab1$modal_res, tab1$modal_res_all) %>%
      arrange(GCA)

  # modal cut score ouput ready
    tab1$modal_table <- gen_indi_table(tab1$modal_res_com)
    tab1$modal_table_all <- gen_indi_table(tab1$modal_res_all)
  # })
#
# for_report <- list() # reactiveValues()
#
  # observeEvent(input$run_tab1, # For Individual Table output
  # {
    # output$indi <-
      # DT::renderDT({

        # for_report$indi <-
          dt_table_out_indi(tab1$indi_table, table_options_new_1)
        # for_report$indi

        # })

    # waitress$close()
  # })
#
  # observeEvent(input$run_tab1, # modal or median cut score output
  # {
    # output$group_median <-
      # DT::renderDT({
#
        # for_report$med <-
          dt_table_out_med(tab1$median_table, table_options_new_2)
        # for_report$med
      # })
  # })

  # observeEvent(input$run_tab1, # modal or median cut score output
  # {
    # output$group_mode <-
      # DT::renderDT({
        # for_report$mode <-
          dt_table_out_mode(tab1$modal_table, table_options_new_2)
        # for_report$mode
        # })
    # })
#
# Detailed ESS Group Results
#-----------------------------------------------------------------------
## Generate TAB & output panels inside generated Tabs
#------------------------------------------------------------------------
  # observeEvent(input$run_tab1,
  # {
    modal_selected_cp <- tab1$modal_selected_cp_all

    n.of.gca <- information$data_ready$id_list$GCA
    n.of.tb <- rep(1, length(n.of.gca))

    test_nm <- n.of.gca
    target_loc <- information$base_data$loc_nm
    target_filter <- information$base_data$target_nm

  # Tab generation + putting uiOutput
    TAB  <-
      do.call(
        tabsetPanel,
        c(id ='tab',lapply(1:length(n.of.gca), function(i) {
          tabPanel(
            title = paste0('GCA: ', test_nm[i]),
            uiOutput(paste0('out',i))
            )
          })
        )
      )
    output$tabs <- renderUI({  TAB  })

  # Place Output for each tab
    v <- vector("list", length(n.of.gca))
    for(vi in 1:length(n.of.gca)) {
      in_num <- n.of.tb[vi]

      for(vvi in 1:in_num){
        txt_outname <- paste("txt", vi, vvi, sep = "_")
        t_outname <- paste("t1", vi, vvi, sep = "_")
        eff_outname <- paste("eff1", vi, vvi, sep = "_")
        p_outname <- paste("p1", vi, vvi, sep = "_")

        ct_outname <- paste("ct", vi, vvi, sep = "_")

        cutpoints <- paste(modal_selected_cp[vi][[vvi]], collapse = ",")

        v[[vi]][[vvi]] <-
          fluidRow(
            column(width = 5, align = 'left',
                textInput(txt_outname, "Minimum point",
                  value = cutpoints),
                htmlOutput(eff_outname),
                htmlOutput(t_outname)
              ),
            column(width = 7, align = 'right',
                htmlOutput(ct_outname),
                plotOutput(p_outname, width = "90%", height = "550px")
              )
            )
      }
    }
  ## Generate outputs inside each uioutput
    lapply(1:length(n.of.gca), function(vi) {
      ui_outname <- paste0("out", vi)
      output[[ui_outname]] <-
        renderUI({
          in_num <- n.of.tb[vi]
          lapply(1:in_num, function(vvi){
            v[[vi]][[vvi]]
          })
        })
    })
  # })


## Obtain Data for Display (Cut Scores) #
#------------------------------------------------------------------------
  tab2 <- list() # reactiveValues()
  # observeEvent(input$run_tab1,
  # {
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

          p1 <- tab2_plot(dataUse_2, information$base_data$WESS_nm)

          list(eff_data = eff_data, t_out = dataUse_1,
               crosst = ct_1, p1 = p1)
        })
      })
  # })

## Output for Detailed ESS Group Results
#------------------------------------------------------------------------
  # observeEvent(input$run_tab1,
  # {
    loc_nm <- information$base_data$loc_nm
    WESS_nm <- information$base_data$WESS_nm

    n.of.gca <- information$data_ready$id_list$GCA
    n.of.tb <- rep(1, length(n.of.gca))

    for_tab2_out <- tab2$for_tab2_out

    # put the results into each output
    lapply(1:length(n.of.gca), function(vi) {
      # vi = 1; vvi = 1
      in_num <- n.of.tb[vi]
      lapply(1:in_num, function(vvi) {

        t_outname <- paste("t1", vi, vvi, sep = "_")
        eff_outname <- paste("eff1", vi, vvi, sep = "_")
        p_outname <- paste("p1", vi, vvi, sep = "_")

        ct_outname <- paste("ct", vi, vvi, sep = "_")

        dataUse_1 <- for_tab2_out[[vi]][[vvi]][["t_out"]]

        crosstabs <- for_tab2_out[[vi]][[vvi]][["crosst"]]

        p1 <- for_tab2_out[[vi]][[vvi]][["p1"]]

        output[[t_outname]] <- renderText( {
          tab2_table(dataUse_1,information$base_data$WESS_nm)
        })

        output[[eff_outname]] <-
          renderText({
          # vi = 1; vvi = 1
          efft_data <-
            for_tab2_out[[vi]][[vvi]][["eff_data"]] %>%
            select(-ends_with("_p")) %>%
            mutate(
              Table = if_else(Table == 0, "All", as.character(Table)),
              Correlation = round(Correlation, 3)
              )

          tab2_table_effpage(efft_data)
        })

        output[[p_outname]] <- renderPlot( {
          p1
        })

        output[[ct_outname]] <- renderText({
          ct_1 <- crosstabs

          num_level <- ncol(ct_1)
          as.data.frame.matrix(ct_1) %>%
            mutate(".." := rownames(.),
                   .before = 1) %>%
            mutate("." := "Operational Level",
                   .before = 1) %>%
            kable() %>%
            kable_styling(
              c("striped"),
              full_width = F,
              font_size = 14
            ) %>%
            column_spec(
              1,
              bold = T,
              width="3em",extra_css="transform: rotate(-90deg);"
              ) %>%
            collapse_rows(., columns = 1, valign = "middle") %>%
            add_header_above( c(" " = 2, "Aligned_ALD" = num_level ))
          })
        })
      })
    # })
#
# Cut Score Summary
#-----------------------------------------------------------------
## Summary of Cut Scores and impact data
#-----------------------------------------------------------------
  tab3 <- list()# reactiveValues()
  # observeEvent(input$run_tab1,
  # {
    target_filter <- information$base_data$target_nm
    target_loc <- information$base_data$loc_nm
    gca_nm <- information$data_ready$id_list$GCA

    setup_data <- information$imported_data$setup_data
    examinee_data <-
      information$imported_data$examinee_data

    gca_p <-
      which(upper_remove_blank(names(examinee_data)) ==
        upper_remove_blank("grade")|
          upper_remove_blank(names(examinee_data)) ==
        upper_remove_blank("gca"))

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

    if(information$base_data$modal_nm == "modal") {
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
  # })

## Summary of Cut Scores and impact data Display #
#------------------------------------------------------------
  # observeEvent(input$run_tab1, {
  #   output$pagetb <-
  #     renderText({
  #       page_data <- tab3$eff_page %>% data.frame()
  #
  #       forline <- page_data %>% pull(1) %>% unique() %>% length()
  #       kable.line <- 1:forline
  #       for(fl in 1:forline){
  #         kable.line[fl] <- 5*fl + 0
  #       }
  #
  #       for_report$effpage <-
  #         page_data %>%
  #         kable(.,"html", escape = F, align = "c",
  #               table.attr = "style='width:50%;'") %>%
  #         kable_styling(bootstrap_options = c("striped"),
  #                       # full_width = F,
  #                       position = "left",
  #                       font_size = 18,
  #                       fixed_thead = T) %>%
  #         row_spec(1:nrow(page_data), color = "black") %>%
  #         row_spec(0, angle = 0,
  #                   background = "floralwhite",
  #                   extra_css = "border-bottom: 1px solid") %>%
  #         collapse_rows(columns = 1:2, valign = "top") %>%
  #         row_spec(., kable.line, extra_css = "border-bottom: 1px solid")
  #
  #       for_report$effpage
  #
  #       })
  # })
  #
  # observeEvent(input$run_tab1,
  # {
  #   output$pagePlot1 <- renderPlot({
  #
  #   tab3$p_page1 <-
  #       tab3$scale_scores %>%
  #       ggplot() +
  #       geom_line(aes(x = GCA, y = scaleScore,
  #         colour = Level, group = Level),size = 2) +
  #       geom_text(aes(label = scaleScore,
  #                   x = GCA, y = scaleScore, group = Level), size = 6,
  #         vjust = 1) +
  #       labs(title = "Scale Score Cut Scores",
  #            y = "Scale Score Cut Scores") +
  #       theme_bw(base_size = 20) +
  #       scale_color_brewer(palette="Paired")
  #
  #     tab3$p_page1
  #   })
  #   output$pagePlot2 <- renderPlot({
  #
  #     tab3$p_page2 <-
  #         tab3$perc_ins %>%
  #         mutate(Level = factor(Level),
  #                Level = factor(Level, levels = rev(levels(Level)))
  #         ) %>%
  #         ggplot() +
  #         geom_col(aes(x = GCA, y = percIn, fill = Level)) +
  #         geom_text(aes(label = percIn,
  #                       x = GCA, y = percIn, group = Level),
  #                   size = 6,
  #                   position = position_stack(vjust = .5)) +
  #         labs(title = "Percentage in Level",
  #              y =  "Percentage in Level") +
  #         theme_bw(base_size = 20) +
  #         scale_fill_brewer(palette="Paired")
  #
  #       tab3$p_page2
  #   })
  #   output$pagePlot3 <- renderPlot({
  #
  #     tab3$p_page3 <-
  #         tab3$perc_atabos %>%
  #         ggplot() +
  #         geom_line(
  #           aes(x = GCA, y = percAtabo, colour = Level, group = Level),
  #           size = 1.5) +
  #         geom_text(aes(label = percAtabo,
  #                       x = GCA, y = percAtabo, group = Level), size = 6,
  #         vjust = 1) +
  #
  #         labs(title = "Percentage At or Above Cut Score",
  #              y = "Percentage At or Above Cut Score") +
  #         theme_bw(base_size = 20) +
  #         scale_color_brewer(palette="Paired")
  #
  #       tab3$p_page3
  #   })
  # })
# #
# Item Review
#----------------------------------------------------------------
## Item Review Ready
#----------------------------------------------------------------
  tab4 <- list() # reactiveValues()
  # observeEvent(input$run_tab1,
  # {
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
  # })

## Item Review Display
#----------------------------------------------------------------
  # observeEvent(input$run_tab1,
  # {
    output$review <-
      DT::renderDataTable({

        maxRow <- nrow(tab4$for_tab4_out)
        grades <- tab4$for_tab4_out %>% pull(1) %>% unique()
        colors <- c('#DAF7A6',"#A6B1F7","#A6F7C3","#A6DAF7","#FFC300")

        tab4_out <-
          tab4$for_tab4_out

        for_report$ireview <-
          DT::datatable(tab4_out,
                      rownames = F,
                      options = table_options_new_2(maxRow)
            ) %>%
          formatStyle(1,
            backgroundColor = styleEqual(grades,colors[1:length(grades)])
            )

        for_report$ireview
      })
   # })
#
# Report
#------------------------------------------------------------------------
  reportTables <- reactiveValues()
  observeEvent(input$run_tab4,
  {
 setup_data <-
      information$imported_data$setup_data %>%
      set_names(nm = c("GCA","Content Area", "Grade","Table per Room","Level Options", "SD"))
    setup_data <-
      classAppend(setup_data, "inpdata")
    reportTables$setup <- mytable(setup_data, "Table 1. Grades, Content Areas, Numbers and Names of Levels")

    panel_data <-
      classAppend(information$data_ready$id_list$PanelID, "inpdata")
    reportTables$panel <- mytable(panel_data, "Table 2. Panelists and Configuration")

    indi_table <- tab1$indi_table %>% rename("Cors" = "Correlation")
    indi_table <- classAppend(indi_table, "ESSresult1")
    reportTables$indi <-
      mytable(indi_table, "Table 3. Inidividual ESS")

    modal_table <- tab1$modal_table %>% rename("Cors" = "Correlation")
    modal_table <- classAppend(modal_table, "ESSresult1")
    reportTables$modal <- mytable(modal_table, "Table 4. Modal ESS")

    median_table <- classAppend(tab1$median_table, "ESSresult2")
    reportTables$med <- mytable(median_table, "Table 5. Median ESS")

    for(i in 1:length(tab2$for_tab2_out)){
      # i <- 1
      f_tab_gen <- tab2$for_tab2_out[[i]][[1]][["t_out"]]
      f_tab_gen <- classAppend(f_tab_gen, "detailESS")
      f_tab_gen <- mytable(f_tab_gen, paste0("Table 6-",i,". Detailed ESS"))
      reportTables[[paste0("detailESS_",i)]] <- f_tab_gen
    }

     for(i in 1:length(tab2$for_tab2_out)){
      # i <- 1
      crosstabs <- tab2$for_tab2_out[[i]][[1]][["crosst"]]
      crosstabs <- classAppend(crosstabs, "crosst")
      crosstabs <- mytable(crosstabs, paste0("Table 7-",i,". Crosstabs"))
      reportTables[[paste0("crosst_",i)]] <- crosstabs
    }

    eff_page <- classAppend(tab3$eff_page, "effpage")
    reportTables$effpage<-mytable(eff_page, "Table 8. Cut Page")

    item_review <- classAppend(tab4$for_tab4_out, "review")
    reportTables$ireview <- mytable(item_review, "Table 9. Item Review")
  })

  observe({
    req(reportTables$ireview)
    show_alert(
      title = "Report Ready",
      text = "Press Download",
      type = "success"
    )
  })


  output$report <- downloadHandler(
    filename = function() {
      paste("Report_",Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      shiny::withProgress(
        message = paste0("Downloading", " the document"),
        value = 0,
        {
          file_docx <- tempfile(fileext = ".docx")
          for_tab2_out <- tab2$for_tab2_out

          shiny::incProgress(1/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)

          word_out3(file_docx, reportTables = reportTables,
            for_tab2_out = for_tab2_out, tab3 = tab3)

          Sys.sleep(1)
          shiny::incProgress(4/10)
          Sys.sleep(1)

          file.rename( from = file_docx, to = file )
        }
      )
    }
  )

# Validity Tab
#-------------------------------------------------------------------
  val_data <-
    reactive({
      read_excel(input$val_data$datapath)
      })
observeEvent(input$val_import,
  {
    SD_data <- information$data_ready$SD_data
    val_data <- val_data()

  cut_score <-
    map(tab3$page_data, ~ .[["scale_scores"]]) %>%
    bind_rows() %>%
    left_join(., SD_data, by = c("GCA" = "GCAid"))

  val_table <-
    tibble(
      GCA = val_data$GCA,
      Level = cut_score$Level,
      Existing_Cut_score = val_data$`Cut Score`,
      ESS_Cut_score =cut_score$scaleScore,
      Difference = Existing_Cut_score - ESS_Cut_score,
      SD = cut_score$SD,
      Std_Diff = round(Difference/SD,2)
      )

  output$valid <-
    DT::renderDataTable({
      DT::datatable(val_table)
      })
  })

})#shinySever
##############################################################
