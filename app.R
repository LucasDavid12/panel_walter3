#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(stringr)
library(htmltools)
library(shinyWidgets)
library(shinycustomloader)
library(DT)



barrios <- readRDS("barrios_popu.RDS") %>% arrange(decada) %>% rename(distrito = Departamento)
barrios_dpto <- readRDS("barrios_depto.RDS")
barrios_table <- readRDS("barrios_table.RDS")
rendimiento <- readRDS("rendimientos.RDS")
obras_mapa <- readRDS("obras_mapa.RDS") %>% rename(Distrito = nombredepto)
obras_table <- readRDS("obras_table.RDS") %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "25 De Mayo", replacement = "Veinticinco de Mayo")) %>% 
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "9 De Julio", replacement = "Nueve de Julio")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Bolívar", replacement = "Bolivar")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Carmen De Areco", replacement = "Carmen de Areco")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Coronel De Marina Leonardo Rosales", replacement = "Coronel Rosales")) %>% 
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Exaltación De La Cruz", replacement = "Exaltación de la Cruz")) %>% 
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "General Juan Madariaga", replacement = "General Madariaga")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "José M. Ezeiza", replacement = "Ezeiza")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Lomas De Zamora", replacement = "Lomas de Zamora")) %>% 
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Puán", replacement = "Puan")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "San Andrés De Giles", replacement = "San Andrés de Giles")) %>%
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "San Antonio De Areco", replacement = "San Antonio de Areco")) %>% 
  mutate(nombredepto = str_replace_all(string = nombredepto, pattern = "Tres De Febrero", replacement = "Tres de Febrero")) %>% 
  rename(Inicio = fechainicioanio, Fin = fechafinanio, 
                                                     Obra = nombreobra, Descripción = descripicionfisica, 
                                                     Monto = montototal, Sector = sectornombre, Ejecutor = entidadejecutoranombre, 
                                                     Dias_de_obra = duracionobrasdias, Objetivo = objetivogeneral, Tipo_de_proyecto = tipoproyecto, 
                                                     Distrito = nombredepto, Etapa = etapaobra, URL = url_perfil_obra) %>% 
    select(Distrito, Inicio, Fin, Obra, Monto, Sector, Ejecutor, Dias_de_obra, Objetivo, Tipo_de_proyecto, Etapa, URL) 
internet <- readRDS("internet.RDS") %>% filter(Provincia == "BUENOS AIRES")
### filtro rendimientos de hospitales 
rendimiento18 <- rendimiento %>% filter(anio == "2018")
rendimiento19 <- rendimiento %>% filter(anio == "2019")
rendimiento20 <- rendimiento %>% filter(anio == "2020")
###rendimiento
rendimientos_table <- as_tibble(rendimiento) %>% select(distrito, anio, consultas_medicas, cosultas_paramedicas, dias_camas_disponibles, pacientes_dias, 
                                                        altas, defunciones, tasa_consultas_m_x100)
obras_table$url_perfil_obra <- paste0("<a href='",obras_table$url_perfil_obra,"' target='_blank'>",obras_table$url_perfil_obra,"</a>")


ui <- dashboardPage(
    dashboardHeader(title = "Panel de control - Buenos Aires", titleWidth = 350), 
    dashboardSidebar(width = 350,
                     sidebarMenu(                                  
                         id = "sidebar", 
                         menuItem(text = "Estructura provincial", tabName = "mamb", icon = icon("route")), 
                         conditionalPanel("input.sidebar == 'mamb' && input.t4 == 'val_barrios'", prettyRadioButtons(inputId = "filter_barrios", "Filtro:", choices = unique(c("Barrios", "Barrios por municipio")))),
                         conditionalPanel("input.sidebar == 'mamb' && input.t4 == 'val_ren'", prettyRadioButtons(inputId = "filter_rendi", label = "Fecha:", c("2018" = "2018", "2019" = "2019", "2020" = "2020"), animation = "pulse"))
                     ),
                     div(style="display:inline-block;width:32%;text-align: center;", actionButton("primera", label = NULL, style = "width: 300px; height: 110px;
background: url('https://lh3.googleusercontent.com/9sNfiMppl_ZzNOhjdwDZK2xvZRwINIHhRSU8UsHPfwW0hxYifkK2Z8ClL7_sygq_ojMm6fquxFuXmVmAikcLX8n7L5slWgoe1NN3E6X4vg0v8xxV2JcLjGlWTi7pZn5haxJ76Y-N2cFGM44Qig');  background-size: cover; background-position: center;"))),                      
    dashboardBody(
        
            tabItem(
                
                
                #pestaña de estructura provincial 
                
                
                tabName = "mamb", 
                tabBox(id = "t4", width = 15, 
                       tabPanel(title = "Barrios populares", value = "val_barrios", withLoader(leafletOutput("mapbarrios"), type="html", loader="pacman"), box(title =  strong("Info. sobre los barrios"), width = "100%", status = "info", solidHeader = T, collapsible = T, withLoader(DT::dataTableOutput("table_barrios", width = "100%")))), 
                       tabPanel(title = "Rendimientos de establecimientos de salud", value = "val_ren", withLoader(leafletOutput("maprendimientos"), type="html", loader="pacman"), box(title =  strong("Info. el rendimiento de los hospitales"), width = "100%", status = "info", solidHeader = T, collapsible = T, withLoader(DT::dataTableOutput("table_rendi", width = "100%")))),
                       tabPanel(title = "Internet por localidad", value = "val_int", withLoader(leafletOutput("mapinternet"), type="html", loader="pacman")),
                       tabPanel(title = "Obras públicas", withLoader(leafletOutput("mapobras"), type="html", loader="pacman"), withLoader(DT::dataTableOutput("table_obras"), type="html", loader="pacman"))))
            
        
    )
)





server <- function(input, output) {
    
    
    output$mapbarrios <- renderLeaflet({ 
        
        popup_barrios <- paste("<B>Distrito</B>: ",  barrios$Localidad, "<BR>",
                             "<B>Decada de creación</B>: ", barrios$decada, "<BR>",
                             "<B>Electridad</B>: ", barrios$Electricidad, "<BR>", 
                             "<B>Cloaca</B>: ", barrios$Cloaca, "<BR>", 
                             "<B>Agua</B>: ", barrios$Agua, "<BR>",
                             "<B>Gas</B>: ", barrios$Gas, "<BR>",
                             "<B>Familias estimadas</B>: ", barrios$Familias.estimadas) %>% lapply(HTML)
        
       
        
        pal_barrios <- colorFactor(c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99',
                                     '#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99',
                                     '#b15928', '#fdb462'), domain = barrios$decada)
        
        pal_barrios_dpto <- colorBin(palette = "Greens", domain = barrios_dpto$`Familias estimadas`, n = 9)
        
        
        
        switch(input$filter_barrios,
               "Barrios" = 
            leaflet() %>% 
            addTiles() %>%
            addPolygons(data = barrios, weight = 1, color = "black", label = 
                            paste0(as.character(barrios$distrito),
                                   " | barrios: ", as.character(barrios$Barrio)),
                        popup = popup_barrios,
                        fillColor = ~pal_barrios(barrios$decada), 
                        fillOpacity = 0.8, 
                        highlight = highlightOptions(weight = 3, 
                                                     color = "black", 
                                                     bringToFront = T), 
                        layerId = ~Barrio) %>%
            addLegend(position = "bottomright", 
                      pal = pal_barrios,
                      values = barrios$decada,
                      title = "Decada de creación"), 
            "Barrios por municipio" = 
              leaflet() %>% 
              addTiles() %>%
              addPolygons(data = barrios_dpto, weight = 1, color = "black", label = 
                            paste0(as.character(barrios_dpto$distrito),
                                   " | Familias estimadas: ", as.character(barrios_dpto$`Familias estimadas`)),
                          fillColor = ~pal_barrios_dpto(barrios_dpto$`Familias estimadas`), 
                          fillOpacity = 0.8, 
                          highlight = highlightOptions(weight = 3, 
                                                       color = "black", 
                                                       bringToFront = T), 
                          layerId = ~distrito) %>%
              addLegend(position = "bottomright", 
                        pal = pal_barrios_dpto,
                        values = barrios_dpto$`Familias estimadas`,
                        title = "Cantidad de familias")
        )
    })
    
    observeEvent(input$mapbarrios_shape_click, {
      
      #capture the info of the clicked polygon
      click <- input$mapbarrios_shape_click
      
      #subset your table with the id of the clicked polygon 
      selected <- barrios_table[barrios_table$distrito == click$id,]
      
      #if click id isn't null render the table
      if(!is.null(click$id)){
        output$table_barrios = DT::renderDataTable({
          DT::datatable(selected, options = list(paging = TRUE, scrollX = T, scrollY = T, 
                                                 mode = "single", target = "cell",
                                                 autoWidth = T, server = F, dom = 'Bfrtip', buttons = c('csv', 'excel'),
                                                 columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                                                   list(targets = c(0, 8, 9), visible = FALSE))
          ), extensions = 'Buttons', 
          selection = 'single', 
          filter = 'top')
        })
      } 
    })
    
    output$maprendimientos <- renderLeaflet({
        
        
        pal_18 <- colorBin(palette = "Purples", domain = rendimiento18$tasa_consultas_m_x100, n = 9)
        
        pal_19 <- colorBin(palette = "Purples", domain = rendimiento18$tasa_consultas_m_x100, n = 9)
        
        pal_20 <- colorBin(palette = "Purples", domain = rendimiento18$tasa_consultas_m_x100, n = 9)
        
        switch(input$filter_rendi,
               "2018" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = rendimiento18, weight = 1, color = "black", label = 
                                   paste0(as.character(rendimiento18$distrito),
                                          " | consultas: ", as.character(format(rendimiento18$consultas_medicas, big.mark = "."))),
                               fillColor = ~pal_18(rendimiento18$tasa_consultas_m_x100), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_18,
                             values = rendimiento18$tasa_consultas_m_x100,
                             title = "Tasa cada 100pers. de consultas medicas"), 
               "2019" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = rendimiento19, weight = 1, color = "black", label = 
                                   paste0(as.character(rendimiento19$distrito),
                                          " | consultas: ", as.character(format(rendimiento19$consultas_medicas, big.mark = "."))),
                               fillColor = ~pal_19(rendimiento19$tasa_consultas_m_x100), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_19,
                             values = agri_2018$tasa_consultas_m_x100,
                             title = "Tasa cada 100pers. de consultas medicas"),
               "2020" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = rendimiento20, weight = 1, color = "black", label = 
                                   paste0(as.character(rendimiento20$distrito),
                                          " | consultas: ", as.character(format(rendimiento20$consultas_medicas, big.mark = "."))),
                               fillColor = ~pal_20(rendimiento20$tasa_consultas_m_x100), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_20,
                             values = rendimiento20$tasa_consultas_m_x100,
                             title = "Tasa cada 100pers. de consultas medicas")
               
               
        )})
    
    observeEvent(input$maprendimientos_shape_click, {
      
      #capture the info of the clicked polygon
      click <- input$maprendimientos_shape_click
      
      #subset your table with the id of the clicked polygon 
      selected <- rendimientos_table[rendimientos_table$distrito == click$id,]
      
      #if click id isn't null render the table
      if(!is.null(click$id)){
        output$table_rendi = DT::renderDataTable({
          DT::datatable(selected, options = list(paging = TRUE, scrollX = T, scrollY = T,
                                                 autoWidth = T, server = F, dom = 'Bfrtip', buttons = c('csv', 'excel'),
                                                 columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                                                   list(targets = c(0, 8, 9), visible = FALSE))
          ), extensions = 'Buttons', 
          selection = 'single', 
          filter = 'top')
        })
      } 
    })
    
    ### obras publicas
    
    output$mapobras <- renderLeaflet({
        
        pal_obras <- colorBin(palette = "YlOrRd", domain = obras_mapa$n, bins = 9)
        
        
        leaflet() %>% 
            addTiles() %>%
            addPolygons(data = obras_mapa, weight = 1, color = "black", label = 
                            paste0(as.character(obras_mapa$Distrito),
                                   " | Cantidad de obras: ", as.character(obras_mapa$n)),
                        fillColor = ~pal_obras(obras_mapa$n), 
                        fillOpacity = 0.8, 
                        highlight = highlightOptions(weight = 3, 
                                                     color = "black", 
                                                     bringToFront = T), 
                        layerId = ~Distrito) %>%
            addLegend(position = "bottomright", 
                      pal = pal_obras,
                      values = obras_mapa$n,
                      title = "Cantidad de obras")
        
    })
    
    observeEvent(input$mapobras_shape_click, {
        
        #capture the info of the clicked polygon
        click <- input$mapobras_shape_click
        
        #subset your table with the id of the clicked polygon 
        selected <- obras_table[obras_table$Distrito == click$id,]
        
        #if click id isn't null render the table
        if(!is.null(click$id)){
            output$table_obras = DT::renderDataTable({
                DT::datatable(selected, escape = F, options = 
                                list(paging = TRUE,scrollX = T, scrollY = T,
                                     autoWidth = T, ordering = F, dom = 'Bfrtip', buttons = c('csv', 'excel'), 
                                     pageLength = 150, 
                                     lengthMenu = c(15,50,100)), 
                              rownames = FALSE, extensions = 'Buttons', selection = 'single')
            })
        } 
    }) 
    
   
    
    output$mapinternet <- renderLeaflet({
        
      popup_internet <- paste("<B>Distrito</B>: ",  internet$Partido, "<BR>",
                             "<B>Cable modem</B>: ", internet$CABLEMODEM, "<BR>",
                             "<B>Fibra optica</B>: ", internet$FIBRAOPTICA, "<BR>", 
                             "<B>Satelital</B>: ",internet$SATELITAL, "<BR>", 
                             "<B>3G</B>: ", internet$X3G, "<BR>",
                             "<B>4G</B>: ", internet$X4G) %>% lapply(HTML)
      

        
        icon_int <- "https://cdn.pixabay.com/photo/2020/06/30/10/23/icon-5355890_960_720.png"
        
        leaflet() %>% 
            addTiles() %>%
            addMarkers(lat = internet$Latitud, lng = internet$Longitud,
                       label = 
                           paste0(as.character(internet$Localidad),
                                  " | Poblacion: ", as.character(internet$Poblacion)),
                       popup = popup_internet,
                       icon = list(
                           iconUrl = icon_int, 
                           iconSize = c(20, 20)
                       ))
    }) 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


