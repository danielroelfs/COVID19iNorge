### COVID-19 I NORGE ########################

#-- Libraries ------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(htmlwidgets)
library(RCurl)
library(shinyWidgets)

library(rvest)
library(pdftools)
library(mapproj)
library(patchwork)
library(normentR)

### Setup ########################

file <- "norge_fylker.json"
#file <- "COVID19iNorge/norge_fylker.json"
mapdata_load <- geojsonio::geojson_read(file, what = "sp")

# Read report
fhi_url <- "https://www.fhi.no/en/id/infectious-diseases/coronavirus/dags--og-ukerapporter/daily-reports-COVID19/"
pdf_links <- read_html(fhi_url) %>%
    html_nodes("a") %>% 
    html_attr('href')
pdf_links <- pdf_links[grepl(".pdf",pdf_links)]
pdf_links <- paste0("https://www.fhi.no",pdf_links)

#-- Get info ------------------------

pdf_text <- pdf_text(pdf_links[1]) %>% .[1]

latest_update_date <- pdf_text %>%
    str_match("[0-9]+.[0-9]+.2020") %>%
    as.Date(format = "%d.%m.%y")

latest_update_time <- pdf_text %>%
    str_match("[0-9]+:[0-9]+") %>%
    .[1]

#-- Get case information ------------------------

cases_total <- pdf_text %>%
    regmatches(regexec("totalt (.*?) tilfeller",.)) %>%
    unlist() %>%
    gsub("\\s", "", .) %>%
    as.numeric() %>% 
    .[2]

cases_new <- pdf_text %>%
    regmatches(regexec("hvorav (.*?) tilfeller",.)) %>%
    unlist() %>%
    as.numeric() %>% 
    .[2]

mean_age <- pdf_text(pdf_links[1]) %>% .[2] %>%
    regmatches(regexec("er (.*?) år",.)) %>%
    unlist() %>%
    str_split(" ") %>%
    unlist() %>%
    as.numeric() %>%
    unique() %>%
    .[!is.na(.)]

sex_perc <- pdf_text(pdf_links[1]) %>% .[2] %>%
    regmatches(regexec("år, (.*?) % er kvinner",.)) %>%
    unlist() %>%
    .[2] %>%
    as.numeric()

#-- Get info per fylke ------------------------

table_fylke <- pdf_text(pdf_links[1]) %>% .[4] %>%
    str_split("\\n") %>%
    unlist() %>% .[6:16] %>%
    str_split(" +")

fylke_cases <- data.frame()
for (i in 1:length(table_fylke)) {
    if (length(table_fylke[[i]]) > 3) {
        fylke_cases[i,"fylke"] <- paste(table_fylke[[i]][2:4], collapse = " ")
    } else {
        fylke_cases[i,"fylke"] <- table_fylke[[i]][2]
    }
    fylke_cases[i,"ncases"] <- as.numeric(table_fylke[[i]][length(table_fylke[[i]])])
}

#-- Get info per age category ------------------------

table_age <- pdf_text(pdf_links[1]) %>% .[2] %>%
    str_split("\\n") %>%
    unlist() %>% .[7:16] %>%
    str_split(" +")

age_cases <- data.frame()
for (i in 1:length(table_age)) {
    age_cases[i,"age1"] <- parse_number(table_age[[i]][2])
    age_cases[i,"age2"] <- age_cases[i,"age1"] + 9
    age_cases[i,"ncases"] <- as.integer(table_age[[i]][length(table_age[[i]])])
}

plotdata_age <- age_cases %>%
    mutate(age_cat = paste(age1, "-", age2, "år"),
           riskgroup = ifelse(age1 >= 70, "risk", "no risk"))

#-- Collect data per fylke ------------------------

trans_names <- data.frame(
    oldname = c("Ãstfold","Akershus","Hedmark","Oppland","Buskerud",
                "Vestfold","Telemark","Aust-Agder","Vest-Agder","Hordaland",
                "Bergen","Sogn og Fjordane","Sør-Trøndelag","Nord-Trøndelag",
                "Troms","Finnmark",
                "Oslo","Rogaland","Møre og Romsdal","Nordland"),
    newname = c("Viken","Viken","Innlandet","Innlandet","Viken",
                "Vestfold og Telemark","Vestfold og Telemark","Agder", "Agder",
                "Vestland","Vestland","Vestland","Trøndelag","Trøndelag",
                "Troms og Finnmark","Troms og Finnmark",
                "Oslo","Rogaland","Møre og Romsdal","Nordland")
)

inh_fylke <- data.frame(
    newname = c("Oslo","Rogaland","Møre og Romsdal","Nordland",
                "Viken","Innlandet","Vestfold og Telemark",
                "Agder","Vestland","Trøndelag","Troms og Finnmark"),
    pop = c(673469,473526,266856,243856,1234374,370994,
            415777,303754,631594,458744,243925)
)

#fylke_nos <- data.frame(
#  newname = c("Oslo","Rogaland","Møre og Romsdal","Nordland","Viken",
#              "Innlandet","Vestfold og Telemark","Agder","Vestland",
#              "Trøndelag","Troms og Finnmark"),
#  no = c(03,11,15,18,30,34,38,42,46,50,54)
#)

fylke_names <- merge(trans_names, inh_fylke, by  = "newname", all.x = TRUE)
#fylke_names <- merge(trans_names, fylke_nos, by = "new", all.x = TRUE)

#-- Merge with map data ------------------------

mapdata_load@data$id <- rownames(mapdata_load@data)

mapdata_fortified <- fortify(mapdata_load, region = "id")
mapdata_merged <- merge(mapdata_fortified, mapdata_load@data, by = "id")
mapdata <- mapdata_merged %>%
    select(id,long,lat,group)

alldata <- mapdata_merged %>%
    rename(oldname = NAME_1) %>%
    merge(fylke_names, by = "oldname", all.x = TRUE) %>%
    merge(fylke_cases %>% rename(newname = fylke), by = "newname", all.x = TRUE) %>%
    mutate(pop_1k = pop/1000,
           pop_100k = pop/100000,
           ncases_1k = ncases/pop_1k,
           ncases_100k = ncases/pop_100k)


### Build App ########################

### UI ###
ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 i Norge", disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        setSliderColor(norment_colors[["grey"]], 1),
        chooseSliderSkin("Flat"),
        
        column(3, 
               h2("COVID-19 i Norge", style = "font-family: 'Lato'; font-weight: 900; color: white;"),
               prettyRadioButtons("trans", "Transformasjon",
                                     c("Ingen" = "identity",
                                       "Log (naturlig)" = "log",
                                       "Log (grunntallet 10)" = "log10")),
               div(id = "per_inh", materialSwitch("perinh", "Vis tilfeller per 100.000 innbyggere", FALSE, 
                                                  right = TRUE, status = "primary")),
               h5("Tilfeller rapportert fra FHI.", style = "color: white;"),
               h5("Oppdateres regelmessig.", style = "color: white;")),
        column(6,plotOutput('map')),
        tableOutput("fylketable"),
        hr(),
        fluidRow(
            valueBoxOutput("CasesTotal", width = 3),
            valueBoxOutput("CasesNew", width = 3),
            valueBoxOutput("MeanAge", width = 3),
            valueBoxOutput("SexPerc", width = 3)
        ),
        hr(),
        fluidRow(
            column(6,plotOutput('age_plot')),
            column(6,plotOutput('fylke_plot'))
        ),
        hr(),
        fluidRow(
            infoBoxOutput("LastUpdate", width = 6)
        )
    ),
    skin = "black",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))
)

### Server ###
server <- function(input, output) {
    
    # Get variables
    trans <- reactive({
        trans <- input$trans
    })
    
    perinh <- reactive({
        perinh <- input$perinh
    })
    
    # Create infoboxes
    Sys.setlocale("LC_TIME", "no_NO")
    upd <- paste(format(latest_update_date, format = "%A %d %B"),latest_update_time)
    
    output$CasesTotal <- renderValueBox({
        valueBox(cases_total, "Tilfeller (totalt) i Norge", icon = icon("heartbeat"))
    })
    
    output$CasesNew <- renderValueBox({
        valueBox(cases_new, "Nye tilfeller i Norge siste døgn", 
                 icon = icon("plus-square"))
    })
    
    output$MeanAge <- renderValueBox({
        valueBox(paste(mean_age, "år"), "Gjennomsnittsalder", 
                 icon = icon("user-clock"), color = "yellow")
    })
    
    output$SexPerc <- renderValueBox({
        valueBox(paste0(sex_perc,"%/",100-sex_perc,"%"), "Distribusjon kjønn (K/M)", 
                 icon = icon("venus-mars"), color = "purple")
    })
    
    output$LastUpdate <- renderValueBox({
        infoBox(upd, "Siste oppdatering", icon = icon("clock"), color = "black")
    })
    
    fylke_table <- alldata %>%
        group_by(newname) %>%
        summarise(ncases = first(ncases),
                  ncases_100k = first(ncases_100k)) %>%
        mutate(ncases = as.integer(ncases),
               ncases_100k = round(ncases_100k,2)) %>%
        arrange(-ncases)
    names(fylke_table) <- c("Fylke","Tilfeller (totalt)","Tilfeller (/100.000inb)")
    
    output$fylketable <- renderTable(fylke_table)
    
    # MAP PLOT
    output$map <- renderPlot({
        
        plotdata <- alldata
        
        if (perinh()) {
            plotdata <- plotdata %>%
                mutate(ncases = ncases_100k)
        }
        
        if (trans() == "identity") {
            plotdata <- plotdata %>%
                mutate(ncases = ncases)
        } else if (trans() == "log") {
            plotdata <- plotdata %>%
                mutate(ncases = log(ncases))
        } else if (trans() == "log2") {
            plotdata <- plotdata %>%
                mutate(ncases = log2(ncases))
        }
        
        mapplot <- ggplot(plotdata, aes(fill = ncases, color = ncases)) +
            geom_map(aes(map_id = id), map = mapdata %>% rename(x = long, y = lat))
        #ggpolypath::geom_polypath(data = mapdata, aes(x = long, y = lat, group = group))  +
        #geom_polygon(data = alldata, aes(x = long, y = lat, group = group, fill = ncases))  +
        #geom_path(color = "red") +
        
        if (perinh()) {
            mapplot <- mapplot +
                scale_color_gradient(low = "grey90", high = "maroon",
                                     guide = guide_colorbar(barwidth = 0.75, barheight = 8, ticks = FALSE)) +
                scale_fill_gradient(low = "grey90", high = "maroon")
        } else {
            mapplot <- mapplot +
                scale_color_gradient(low = "grey90", high = "maroon", trans = trans(),
                                     guide = guide_colorbar(barwidth = 0.75, barheight = 8, ticks = FALSE)) +
                scale_fill_gradient(low = "grey90", high = "maroon", trans = trans())
        }
        
        mapplot <- mapplot +
            coord_map(projection = "conic", lat0 = 60) +
            expand_limits(x = c(4.15,31.5), y = c(57.4,71.6)) +
            #coord_fixed(ratio = 2) +
            theme_norment(base_size = 12, grid = FALSE, axis = FALSE) +
            theme(legend.position = c(0.7,0.3), 
                  text = element_text(color = "grey90"),
                  title = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  plot.background = element_rect(fill = "#303030", colour = "transparent"),
                  panel.background = element_rect(fill = "#303030", color = "transparent"),
                  plot.margin = margin(-1.5, -2, -1.5, -2, "cm"))
        
        print(mapplot)
    }, 
    bg = "#303030"
    )
    
    output$age_plot <- renderPlot({
        
        ageplot <- ggplot(plotdata_age, aes(x = age_cat, y = ncases, fill = ncases)) +
            geom_col(fill = "grey90") + 
            geom_text(aes(y = ncases + 15, label = ncases), color = "grey90", size = 6) +
            #labs(caption = "Risikogrupper vist i rødt") +
            scale_y_continuous(limits = c(0,max(plotdata_age$ncases + 20)),
                               trans = "identity") +
            #scale_fill_manual(values = c("grey90","maroon")) +
            #scale_fill_gradient(low = "maroon", high = "grey90") +
            theme_norment(axis_text_size = 14, caption_size = 12, axis_title_size = 18, grid = FALSE) +
            theme(legend.position = "none", 
                  text = element_text(color = "grey90"),
                  title = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(color = "grey90", angle = 30, hjust = 1),
                  axis.text.y = element_blank(),
                  plot.background = element_rect(fill = "#303030", colour = "transparent"),
                  panel.background = element_rect(fill = "#303030", color = "transparent"),
                  #plot.margin = margin(0, 0, 0, 0, "pt")
            )
        
        #pp <- ageplot + fylkeplot + 
        #    plot_annotation(theme = theme(plot.margin = margin(0)))
        print(ageplot)
    }, 
    bg = "#303030"
    )
    
    output$fylke_plot <- renderPlot({
        
        plotdata_fylke <- alldata %>%
            group_by(newname) %>%
            summarise(ncases = first(ncases),
                      ncases_100k = first(ncases_100k))
        
        if (perinh()) {
            plotdata_fylke <- plotdata_fylke %>%
                mutate(ncases = ncases_100k)
            text_yadj <- 2.5
            ax_yadj <- 3
            cap_text <- "Tilfeller per 100.000 innbyggere"
        } else{
            text_yadj <- 15
            ax_yadj <- 20
            cap_text <- "Tilfeller (totalt)"
        }
        
        fylkeplot <- ggplot(plotdata_fylke, aes(x = reorder(newname,-ncases), y = ncases, fill = ncases)) +
            geom_col(fill = "grey90") + 
            geom_text(aes(y = ncases + text_yadj, label = round(ncases,2)), color = "grey90", size = 6) +
            labs(caption = cap_text) +
            scale_y_continuous(limits = c(0,max(plotdata_fylke$ncases + ax_yadj)),
                               trans = "identity") +
            scale_fill_gradient2(low = "grey90", high = "maroon") +
            theme_norment(axis_text_size = 14, caption_size = 12, axis_title_size = 18, grid = FALSE) +
            theme(legend.position = "none", 
                  text = element_text(color = "grey90"),
                  title = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_text(color = "grey90", angle = 30, hjust = 1),
                  axis.text.y = element_blank(),
                  plot.background = element_rect(fill = "#303030", colour = "transparent"),
                  panel.background = element_rect(fill = "#303030", color = "transparent"),
                  #plot.margin = margin(0, 0, 0, 0, "pt")
            )
        
        print(fylkeplot)
    }, 
    bg = "#303030"
    )
    
    # AGE + FYLKE PLOT
    # output$age.fylke_plot <- renderPlot({
    #     
    #     ageplot <- ggplot(plotdata_age, aes(x = age_cat, y = ncases, fill = riskgroup)) +
    #         geom_col() + 
    #         geom_text(aes(y = ncases + 15, label = ncases), color = "grey90", size = 6) +
    #         labs(caption = "Risikogrupper vist i rødt") +
    #         scale_y_continuous(limits = c(0,max(plotdata_age$ncases + 20)),
    #                            trans = "identity") +
    #         scale_fill_manual(values = c("grey90","maroon")) +
    #         theme_norment(axis_text_size = 14, caption_size = 12, axis_title_size = 18, grid = FALSE) +
    #         theme(legend.position = "none", 
    #               text = element_text(color = "grey90"),
    #               title = element_blank(),
    #               axis.title.x = element_blank(),
    #               axis.title.y = element_blank(),
    #               axis.text.x = element_text(color = "grey90", angle = 30, hjust = 1),
    #               axis.text.y = element_blank(),
    #               plot.background = element_rect(fill = "#303030", colour = "transparent"),
    #               panel.background = element_rect(fill = "#303030", color = "transparent"),
    #               #plot.margin = margin(0, 0, 0, 0, "pt")
    #         )
    #     
    #     plotdata_fylke <- alldata %>%
    #         group_by(newname) %>%
    #         summarise(ncases = first(ncases),
    #                   ncases_100k = first(ncases_100k))
    #     
    #     if (perinh()) {
    #         plotdata_fylke <- plotdata_fylke %>%
    #             mutate(ncases = ncases_100k)
    #         text_yadj <- 2.5
    #         ax_yadj <- 3
    #         cap_text <- "Tilfeller per 100.000 innbyggere"
    #     } else{
    #         text_yadj <- 15
    #         ax_yadj <- 20
    #         cap_text <- "Tilfeller (totalt)"
    #     }
    #     
    #     fylkeplot <- ggplot(plotdata_fylke, aes(x = reorder(newname,-ncases), y = ncases, fill = ncases)) +
    #         geom_col() + 
    #         geom_text(aes(y = ncases + text_yadj, label = round(ncases,2)), color = "grey90", size = 6) +
    #         labs(caption = cap_text) +
    #         scale_y_continuous(limits = c(0,max(plotdata_fylke$ncases + ax_yadj)),
    #                            trans = "identity") +
    #         scale_fill_gradient2(low = "grey90", high = "maroon") +
    #         theme_norment(axis_text_size = 14, caption_size = 12, axis_title_size = 18, grid = FALSE) +
    #         theme(legend.position = "none", 
    #               text = element_text(color = "grey90"),
    #               title = element_blank(),
    #               axis.title.x = element_blank(),
    #               axis.title.y = element_blank(),
    #               axis.text.x = element_text(color = "grey90", angle = 30, hjust = 1),
    #               axis.text.y = element_blank(),
    #               plot.background = element_rect(fill = "#303030", colour = "transparent"),
    #               panel.background = element_rect(fill = "#303030", color = "transparent"),
    #               #plot.margin = margin(0, 0, 0, 0, "pt")
    #         )
    #     
    #     #pp <- ageplot + fylkeplot + 
    #     #    plot_annotation(theme = theme(plot.margin = margin(0)))
    #     print(ageplot)
    #     print(fylkeplot)
    # }, 
    # bg = "#303030"
    # )
    
}


### Run App ###
shinyApp(ui = ui, server = server)
