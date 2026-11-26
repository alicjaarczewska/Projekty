
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(purrr)
library(stringr)
library(sf)
library(leaflet)
library(scales)



files <- list(
  "2025" = "Ucz_klasy_dyscypliny_sportowe_województwo_typjedn_rokszkolny20252026.xlsx",
  "2024" = "Ucz_klasy_dyscypliny_sportowe_województwo_typjedn_rokszkolny20242025.xlsx",
  "2023" = "Ucz_klasy_dyscypliny_sportowe_województwo_typjedn_rokszkolny20232024.xlsx",
  "2022" = "Ucz_klasy_dyscypliny_sportowe_województwo_typjedn_rokszkolny20222023.xlsx",
  "2021" = "Ucz_klasy_dyscypliny_sportowe_województwo_typjedn_rokszkolny20212022.xlsx",
  "2020" = "Ucz_klasy_dyscypliny_sportowe_województwo_typjedn_rokszkolny20202021.xlsx"
)

sport_szkoly <- imap_dfr(files, ~{
  df <- read_excel(.x)
  df$rok_id <- .y
  
  names(df) <- names(df) %>%
    str_trim() %>%
    str_replace_all(" ", "_")
  
  df
}) %>%
  mutate(
    rok_id = as.character(rok_id),
    lb_uczniow = as.numeric(lb_uczniow)
  )

sport_szkoly$dyscyplina_sportowa <- ifelse(
  !is.na(sport_szkoly$Dyscyplina_sportowa),
  sport_szkoly$Dyscyplina_sportowa,
  sport_szkoly$DyscyplinaSportowa
)

sport_szkoly <- sport_szkoly %>%
  
  group_by(RSPO) %>%
  
  mutate(
    
    dyscyplina_sportowa = case_when(
      dyscyplina_sportowa %in% c("Pływanie", "Sport pływacki") ~ "Pływanie",
      TRUE ~ dyscyplina_sportowa
    ),
    
    typ_szkoly = case_when(
      
      
      any(Klasa %in% c("VI", "VII", "VIII"), na.rm = TRUE) ~ 
        "Szkoła podstawowa",
      
      TRUE ~ "Szkoła średnia"
    )
  ) %>% 
  
  mutate(
    rodzaj_szkoly = case_when(
      typ_szkoly == "Szkoła średnia" & Typ_podmiotu == "Technikum" ~ "Technikum",
      
      typ_szkoly == "Szkoła podstawowa" ~ "Szkoła podstawowa",
      
      typ_szkoly == "Szkoła średnia" & any(Klasa %in% c("V"), na.rm = TRUE) ~ "Technikum",
      
      TRUE ~ "Liceum ogólnokształcące"
    )
  ) %>% 
  
  ungroup() %>%
  
  filter(
    !is.na(lb_uczniow),
    !is.na(dyscyplina_sportowa),
    !is.na(Klasa)
    
  ) 



ranking_year <- sport_szkoly %>%
  group_by(rok_id, dyscyplina_sportowa) %>%
  summarise(liczba_osob = sum(lb_uczniow), .groups = "drop")

ranking_all <- sport_szkoly %>%
  group_by(dyscyplina_sportowa) %>%
  summarise(liczba_osob = sum(lb_uczniow), .groups = "drop") %>%
  slice_max(liczba_osob, n = 10)


populacja_dane <- data.frame(
  JPT_NAZWA_ = c("dolnośląskie", "kujawsko-pomorskie", "lubelskie", "lubuskie", 
                 "łódzkie", "małopolskie", "mazowieckie", "opolskie", 
                 "podkarpackie", "podlaskie", "pomorskie", "śląskie", 
                 "świętokrzyskie", "warmińsko-mazurskie", "wielkopolskie", "zachodniopomorskie"),
  mieszkancy = c(2868000, 2007000, 2010000, 973000, 
                 2363000, 3429000, 5509000, 938000, 
                 2071000, 1135000, 2359000, 4312000, 
                 1170000, 1357000, 3486000, 1633000)
)



polska <- st_read("C:\\Users\\basub\\Desktop\\ala\\wded\\A01_Granice_wojewodztw.shp", quiet = TRUE)



ciekawostki <- list(
  "dolnośląskie" = "DOLNOŚLĄSKIE – BIATHLON + BRYDŻ: 
Dolny Śląsk łączy sport fizyczny z intelektualnym – biathlon rozwija wytrzymałość i koncentrację młodzieży,
  a brydż od lat jest silnie obecny w szkołach jako narzędzie kształtujące logiczne myślenie. 
  Region ma dobre zaplecze zimowe, a tradycje brydżowe sięgają tu środowisk akademickich i klubowych.",
  
  "kujawsko-pomorskie" = "KUJAWSKO-POMORSKIE – WIOŚLARSTWO + KAJAKARSTWO: 
Dostęp do Wisły i licznych jezior sprawia, że sporty wodne są naturalnym wyborem dla uczniów. 
  Region od dekad wychowuje medalistów wioślarskich i kajakarskich, a szkolne kluby często stanowią 
  pierwszy krok do kariery sportowej.",
  
  "lubelskie" = "LUBELSKIE – BILARD + BOCCIA: 
Lubelskie wyróżnia się podejściem inkluzywnym – boccia rozwija się jako sport dostępny dla wszystkich,
  w tym osób z niepełnosprawnościami. Bilard natomiast zyskuje popularność wśród młodzieży jako dyscyplina 
  wymagająca precyzji i cierpliwości, obecna w lokalnych klubach.",
  
  "lubuskie" = "LUBUSKIE – TENIS STOŁOWY: 
Region stawia na wszechstronność – tenis stołowy jest łatwo dostępny w szkołach. Lubuskie od lat wspiera 
  szkolenie młodzieży w mniej oczywistych, ale wymagających dyscyplinach.",
  
  "łódzkie" = "ŁÓDZKIE – BOKS OLIMPIJSKI: 
Łódź ma silne tradycje bokserskie, które przekładają się na zainteresowanie młodzieży tym sportem już od 
  poziomu szkolnego. Boks olimpijski uczy dyscypliny, odporności psychicznej i pracy nad sobą, a region 
  wychował wielu znanych zawodników.",
  
  "małopolskie" = "MAŁOPOLSKIE – SNOWBOARD: 
Zakopane jako jeden z głównych ośrodków sportów zimowych w Polsce przyciąga młodzież zainteresowaną 
  snowboardem. Sport ten rozwija się dynamicznie dzięki infrastrukturze i tradycjom narciarskim regionu, 
  które naturalnie rozszerzyły się o nowoczesne formy aktywności.",
  
  "mazowieckie" = "MAZOWIECKIE – PIĘCIOBÓJ NOWOCZESNY: 
Mazowsze, z Warszawą na czele, oferuje szerokie zaplecze treningowe dla pięcioboju nowoczesnego. Młodzież 
  ma dostęp do różnorodnych obiektów, co sprzyja rozwijaniu tej wymagającej, wielodyscyplinarnej aktywności.",
  
  "opolskie" = "OPOLSKIE – BADMINTON: 
Opolskie wyróżnia się wysokim poziomem badmintona, ustępując jedynie czołowym regionom w kraju. Dyscyplina ta
  jest popularna w szkołach dzięki swojej dostępności i dynamicznemu charakterowi, a lokalne kluby regularnie 
  szkolą młodych zawodników.",
  
  "podkarpackie" = "PODKARPACKIE – NARCIARSTWO + GOLF: 
Podkarpacie łączy tradycję sportów zimowych z rosnącą popularnością golfa. Narciarstwo jest naturalne dla 
  regionu górskiego, natomiast golf rozwija się jako nowoczesna dyscyplina ucząca precyzji i koncentracji.",
  
  "podlaskie" = "PODLASKIE – ŁYŻWIARSTWO SZYBKIE: 
Białystok stał się ważnym ośrodkiem short tracku dzięki funkcjonowaniu Szkoły Mistrzostwa Sportowego. 
  Młodzież ma tu możliwość rozwijania się w dynamicznej odmianie łyżwiarstwa szybkiego, co jest ewenementem
  na skalę kraju.",
  
  "pomorskie" = "POMORSKIE – PIŁKA RĘCZNA + ŻEGLARSTWO: 
Bliskość morza sprzyja rozwojowi żeglarstwa już od najmłodszych lat, a piłka ręczna cieszy się dużą 
  popularnością w szkołach. Region ma bogate tradycje klubowe, które przekładają się na wysoki poziom 
  szkolenia młodzieży.",
  
  "śląskie" = "ŚLĄSKIE – BASEBALL I SOFTBALL: 
Śląsk to jedyne miejsce w Polsce, gdzie baseball i softball rozwijają się na większą skalę. 
  Młodzież ma tu unikalną możliwość trenowania niszowych dyscyplin, które przyciągają swoją odmiennością 
  i zespołowym charakterem.",
  
  "świętokrzyskie" = "ŚWIĘTOKRZYSKIE – BRAK DOMINUJĄCEJ DYSCYPLINY: 
Region wciąż buduje swoją tożsamość sportową i zmaga się z niższym poziomem usportowienia młodzieży. 
  To jednocześnie szansa na rozwój nowych inicjatyw i inwestycji w aktywność fizyczną w szkołach.",
  
  "warmińsko-mazurskie" = "WARMIŃSKO-MAZURSKIE – LEKKOATLETYKA: 
Naturalne warunki sprzyjają uprawianiu lekkiej atletyki, która jest podstawą wszechstronnego rozwoju 
  młodzieży. Region od lat organizuje liczne zawody szkolne i rozwija talenty w biegach oraz konkurencjach 
  technicznych.",
  
  "wielkopolskie" = "WIELKOPOLSKIE – HOKEJ NA TRAWIE + SZERMIERKA: 
Wielkopolska to kolebka polskiego hokeja na trawie, z długą tradycją klubową i szkolną. Szermierka 
  natomiast rozwija refleks i dyscyplinę, będąc jedną z bardziej prestiżowych dyscyplin dostępnych 
  dla młodzieży.",
  
  "zachodniopomorskie" = "ZACHODNIOPOMORSKIE – GIMNASTYKA: 
Region wyróżnia się silnym zapleczem gimnastycznym, które sprzyja rozwojowi sprawności ogólnej 
  już od najmłodszych lat. Gimnastyka stanowi fundament dla wielu innych sportów i jest często 
  pierwszym krokiem w sportowej ścieżce uczniów."
)


# UI

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    
    /* CAŁE TŁO */
    body {
      background-color: #fff1db;
      color: #3a405a;
      font-family: 'Segoe UI';
    }
    
    /* PANEL BOCZNY */
    .well {
      background-color: #e9c3a3;
      border: #e9c3a3;
      border-radius: 40px;
      box-shadow: none;
    }
    
    /* TABS */
    .nav-tabs > li > a {
      background-color: #3a405a;
      color: #aec5eb;
      border-radius: 8px 8px 0 0;
      margin-right: 4px;
    }
    
    .nav-tabs > li.active > a {
      color: #3a405a;
      background-color: #aec5eb;
    }
    
    /* obramowanie całego panelu z tabsami */

    .tab-content {
      border: 5px solid #aec5eb;
      border-top: 5px solid #aec5eb;
      border-radius: 0 0 12px 12px;
      background-color: #fff1db;
      padding: 15px;
    }
    
    /* TYTUŁ */
    .title-panel {
      color: #3a405a;
    }
    
    h1, h2, h3 {
      color: #3a405a;
      font-weight: 600;
    }
    
    /* SUWAK */

    .irs-bar {
      background: #7483a3 !important;
      border-top: 1px solid #aec5eb !important;
      border-bottom: 1px solid #aec5eb !important;
    }

    .irs-single,
    .irs-from,
    .irs-to {
      background: #7483a3 !important;
    }

    .irs-handle {
      border-color: #7483a3 !important;
      backgroun: #aec5eb !important;
    }


    /* SELECT INPUT */

    .selectize-input {
      background-color: #fffaf2 !important;
      border: none;
      border-radius: 10px;
    }
    
    .selectize-input .item {
      background-color: #fffaf2;
      color: #3a405a;
      border-radius: 6px;
    }

    .selectize-dropdown {
      background-color: #fffaf2 !important;
    }

    .selectize-dropdown .active {
      background-color: #7483a3 !important;
      color: white !important;
    }
    
    .selectize-dropdown .selected {
      background-color: #ffe0a8 ;
      color: #3a405a ;
    }
    
    /* LEGENDA LEAFLET */
    .info.legend {
      background: white;
      padding: 10px;
      border-radius: 10px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    }
    
  "))
  ),
  
  titlePanel("SPORT WŚRÓD POLSKIEJ MŁODZIEŻY"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(
        "rok",
        "Zakres lat:",
        min = 2020,
        max = 2025,
        value = c(2020, 2025),
        step = 1,
        sep = ""
      ),
      
      selectInput(
        "sport",
        "Dyscyplina:",
        choices = c(
          "Wszystkie sporty",
          sort(unique(sport_szkoly$dyscyplina_sportowa))
        ),
        selected = "Wszystkie sporty"
      ),
      
      selectInput(
        "typ_szkoly",
        "Typ szkoły:",
        choices = c("Wszystkie", "Szkoła podstawowa", "Szkoła średnia"),
        selected = "Wszystkie"
      ),
      
      br(),
      br(),
      
      h4("Ciekawostka o województwie:"),
      
      textOutput("ciekawostka"),
      
      br(),
      br(),
      
      h6("* Ważne aby zwrócić uwagę na zmieniającą się skalę na wykresach")
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Opis projektu", 
          h3("Temat"),
          p(HTML("Zajmowaliśmy się badaniem danych pochodzących ze strony:
          <u>https://dane.gov.pl/pl/dataset/2471,uczniowie-wedlug-dyscyplin-sportowych</u>. 
                 Dane te opisują jakie sporty uprawia młodzież szkolna na przestrzeni lat 2020-2025.")),
          br(),
          h4("Wykres 1 - Najpopularniejsze sporty"),
          p("Na wykresie przedstawione jest Top 10 najpopularnieszych sportów w szkołach przez badany okres. 
            Wykres jest zależny od wyboru zakresu lat, oraz dyscypliny - zaznaczony sport pojawia się na 
            wykresie w celu porównania jego popularności do innych."),
          br(),
          h4("Wykres 2 - Podział na klasy"),
          p("Ten wykres ukazuje jak poszczególne dyscypliny rozkładają się między różne grupy wiekowe.
            W zależności od wyboru dyscypliny i zakresu lat ukazuje on różne wartości. Różne kolory 
            pozwalają na czytelne rozróżnienie szkół podstawowych od średnich."),
          br(),
          h4("Wykres 1 - Mapa sportu"),
          p("Na mapie widoczny jest rozkład młodzieży uprawiającej sport w całej polsce. Tutaj na wyświetlane
            informacje wpływa wybór zakresu lat, dyscypliny i typu szkoły. Po kliknięciu na wybrane województwo
            w panelu po lewej stronie wyświetla się ciekawostka do niego dobrana.")
        ),
        
        tabPanel("Najpopularniejsze sporty",
                 plotlyOutput("plot", height = "600px")),
        
        tabPanel("Podział na klasy",
                 plotlyOutput("plot_classes", height = "600px")),
        
        tabPanel("Mapa sportu",
                 leafletOutput("mapa", height = 600)),
        
        tabPanel(
          "Wnioski",
          
          h3("Najważniejsze wnioski"),
          
          tags$ul(
            tags$li("Analiza popularności dyscyplin potwierdza pozycję piłki nożnej jako lidera, 
                    który znacząco wyprzedza każdą inną dyscyplinę w zestawieniu."),
            br(),
            tags$li("Z analizy mapy wynika jednak, że w coraz większej liczbie województw piłka
                    nożna traci swoją dotychczasową dominację na rzecz innych dyscyplin sportowych 
                    (na przykład piłki siatkowej lub pływania)."),
            br(),
            tags$li("Porównując wykres Podziału na klasy możemy dostrzec wyjątkowość pewnych dyscyplin 
                    (m. in. piłki nożnej lub siatkowej), które posiadają unikalną, silnie ustrukturyzowaną 
                    infrastrukturę klubową (ligi młodzieżowe, system rozgrywek), która pozwala utrzymać
                    zawodników nawet wtedy, gdy zmieniają oni szkołę lub środowisko edukacyjne. "),
            br(),
            tags$li("Spadek ogólnej liczby sportowców po przejściu do szkoły średniej sugeruje, że wiele
                    innych dyscyplin jest uprawianych w modelu szkolnym (SKS-y, zajęcia pozalekcyjne przy
                    szkole). Z chwilą ukończenia podstawówki, te aktywności wygasają, podczas gdy wyżej 
                    wymioenione dyscypliny funkcjonują niezależnie od budynków szkolnych.")
        )
        )
      )
    )
  )
)


# SERVER

server <- function(input, output) {
  
  dane_filtrowane <- reactive({
    
    dane <- sport_szkoly %>%
      filter(
        as.numeric(rok_id) >= input$rok[1],
        as.numeric(rok_id) <= input$rok[2]
      )
    
    if (input$typ_szkoly != "Wszystkie") {
      dane <- dane %>%
        filter(typ_szkoly == input$typ_szkoly)
    }
    
    dane <- dane %>%
      filter(dyscyplina_sportowa == input$sport)
    
    dane
  })
 
  # WYKRES 1
  
  output$plot <- renderPlotly({
    
    df_all <- sport_szkoly %>%
      group_by(dyscyplina_sportowa) %>%
      summarise(
        liczba_osob = sum(lb_uczniow, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    df_top <- sport_szkoly %>%
      filter(
        as.numeric(rok_id) >= input$rok[1],
        as.numeric(rok_id) <= input$rok[2]
      ) %>%
      group_by(dyscyplina_sportowa) %>%
      summarise(
        liczba_osob = sum(lb_uczniow, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      slice_max(liczba_osob, n = 10, with_ties = FALSE)
    
    df_selected <- sport_szkoly %>%
      filter(
        dyscyplina_sportowa == input$sport,
        as.numeric(rok_id) >= input$rok[1],
        as.numeric(rok_id) <= input$rok[2]
      ) %>%
      group_by(dyscyplina_sportowa) %>%
      summarise(
        liczba_osob = sum(lb_uczniow, na.rm = TRUE),
        .groups = "drop"
      )
    
    visible_sports <- union(
      df_top$dyscyplina_sportowa,
      df_selected$dyscyplina_sportowa
    )
    
    df_all <- df_all %>%
      filter(dyscyplina_sportowa %in% visible_sports)
    
    df_top <- df_top %>%
      filter(dyscyplina_sportowa %in% visible_sports)
    
    df_selected <- df_selected %>%
      filter(dyscyplina_sportowa %in% visible_sports)
    
    p <- ggplot() +
      
      
      geom_col(
        data = df_all,
        aes(
          x = reorder(dyscyplina_sportowa, liczba_osob),
          y = liczba_osob,
          fill = "Wszystkie lata",
          text = paste0(
            dyscyplina_sportowa,
            "<br>Liczba uczniów przez wszystkie lata: ",
            comma(liczba_osob)
          )
        ),
        alpha = 0.2
      ) +
      
      geom_col(
        data = df_top,
        aes(
          x = reorder(dyscyplina_sportowa, liczba_osob),
          y = liczba_osob,
          fill = "TOP 10",
          text = paste0(
            dyscyplina_sportowa,
            "<br>Liczba uczniów: ",
            comma(liczba_osob)
          )
        ),
        alpha = 0.9
      ) +
      
      geom_col(
        data = df_selected,
        aes(
          x = reorder(dyscyplina_sportowa, liczba_osob),
          y = liczba_osob,
          fill = "Wybrany sport",
          text = paste0(
            dyscyplina_sportowa,
            "<br>Liczba uczniów: ",
            comma(liczba_osob)
          )
        ),
        alpha = 1
      ) +
      
      coord_flip() +
      
      scale_fill_manual(values = c(
        "Wszystkie lata" = "grey65",
        "TOP 10" = "#e27350",
        "Wybrany sport" = "#b83d43"
      )) +
      
      scale_y_continuous(labels = comma) +
      
      labs(
        x = NULL,
        y = "Liczba uczniów",
        fill = NULL
      ) +
      
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#fff1db", color = NA),
        panel.background = element_rect(fill = "#fff1db", color = NA),
        
        panel.grid.major = element_line(color = "#f3dfc2", linewidth = 0.4),
        panel.grid.minor = element_line(color = "#f8ead4", linewidth = 0.2),
        
        axis.line = element_line(color = "#3a405a"),
        axis.text.x = element_text(color = "#3a405a", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#3a405a"),
        axis.title = element_text(color = "#3a405a"),
        
        legend.text = element_text(color = "#3a405a"),
        legend.background = element_rect(fill = "#fff1db", color = NA)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  #WYKRES 2
  
  output$plot_classes <- renderPlotly({
    
    df <- sport_szkoly %>%
      filter(
        as.numeric(rok_id) >= input$rok[1],
        as.numeric(rok_id) <= input$rok[2]
      )
    
    if (input$sport != "Wszystkie sporty") {
      df <- df %>%
        filter(dyscyplina_sportowa == input$sport)
    }
    
    
    df <- df %>%
      mutate(
        Klasa = str_trim(as.character(Klasa)),
        typ_szkoly = as.character(typ_szkoly)
      ) %>%
      filter(!is.na(Klasa), Klasa != "", !is.na(typ_szkoly))
    
    df_plot <- df %>%
      mutate(
        klasa_grupa = case_when(
          typ_szkoly == "Szkoła podstawowa" ~ paste0("SP", Klasa),
          typ_szkoly == "Szkoła średnia" ~ paste0("LO", Klasa),
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(klasa_grupa)) %>%
      group_by(klasa_grupa, rodzaj_szkoly) %>%
      summarise(
        liczba = sum(lb_uczniow, na.rm = TRUE),
        .groups = "drop"
      )
    
    order_levels <- c(
      "SPI","SPII","SPIII","SPIV","SPV","SPVI","SPVII","SPVIII",
      "LOI","LOII","LOIII","LOIV", "LOV"
    )
    
    df_plot$klasa_grupa <- factor(df_plot$klasa_grupa, levels = order_levels)
    
    
    p <- ggplot(df_plot, aes(
      x = klasa_grupa,
      y = liczba,
      fill = rodzaj_szkoly,
      text = paste0(
        "Typ Szkoły: ", rodzaj_szkoly,
        "<br>Liczba uczniów: ", liczba,
        
        if (input$sport != "Wszystkie sporty") {
          paste0(
            "<br>Wybrany sport ",
            ": ",
            input$sport
          )
        } 
      )
    )) +
      
      geom_col() +
      
      scale_fill_manual(values = c(
        "Szkoła podstawowa" = "#e27350",
        "Liceum ogólnokształcące" = "#b83d43",
        "Technikum" = "#96252c"
      )) +
      
      labs(
        x = "Klasa",
        y = "Liczba uczniów",
        fill = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.background = element_rect(fill = "#fff1db", color = NA),
        panel.background = element_rect(fill = "#fff1db", color = NA),
        
        panel.grid.major = element_line(color = "#f3dfc2", linewidth = 0.4),
        panel.grid.minor = element_line(color = "#f8ead4", linewidth = 0.2),
        
        axis.line = element_line(color = "#3a405a"),
        axis.text.x = element_text(color = "#3a405a", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#3a405a"),
        axis.title = element_text(color = "#3a405a"),
        
        legend.text = element_text(color = "#3a405a"),
        legend.background = element_rect(fill = "#fff1db", color = NA)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # MAPA 
  
  output$mapa <- renderLeaflet({
    
    dane <- sport_szkoly %>%
      filter(
        as.numeric(rok_id) >= input$rok[1],
        as.numeric(rok_id) <= input$rok[2]
      )
    
    if (input$typ_szkoly != "Wszystkie") {
      dane <- dane %>%
        filter(typ_szkoly == input$typ_szkoly)
    }
    if (input$sport != "Wszystkie sporty") {
      dane <- dane %>%
        filter(dyscyplina_sportowa == input$sport)
    }
    
    woj_data <- dane %>%
      group_by(Wojewodztwo) %>%
      summarise(
        liczba_osob = sum(lb_uczniow, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Wojewodztwo = tolower(Wojewodztwo)) %>%
      left_join(
        populacja_dane,
        by = c("Wojewodztwo" = "JPT_NAZWA_")
      ) %>%
      mutate(
        wskaznik = (liczba_osob / mieszkancy) * 10000
      )
    
    
    top_sport <- dane %>%
      filter(!is.na(dyscyplina_sportowa)) %>%
      group_by(Wojewodztwo, dyscyplina_sportowa) %>%
      summarise(
        liczba_sport = sum(lb_uczniow, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(Wojewodztwo) %>%
      slice_max(
        liczba_sport,
        n = 1,
        with_ties = FALSE
      ) %>%
      ungroup() %>%
      mutate(
        Wojewodztwo = tolower(Wojewodztwo)
      )
    
    
    mapa_data <- polska %>%
      left_join(
        woj_data,
        by = c("JPT_NAZWA_" = "Wojewodztwo")
      ) %>%
      left_join(
        top_sport,
        by = c("JPT_NAZWA_" = "Wojewodztwo")
      ) %>%
      mutate(
        liczba_osob = coalesce(liczba_osob, 0)
      ) %>%
      st_transform(4326) %>% 
      mutate(
        liczba_osob = coalesce(liczba_osob, 0),
        wskaznik = coalesce(wskaznik, 0)
      )
    
    
    breaks <- pretty(
      mapa_data$wskaznik,
      n = 6
    )
    
    breaks[length(breaks)] <- Inf
    
    labels <- c(
      paste0(
        round(head(breaks, -2), 1),
        "–",
        round(breaks[2:(length(breaks)-1)], 1)
      ),
      paste0(
        round(breaks[length(breaks)-1], 1),
        "+"
      )
    )
    
    mapa_data <- mapa_data %>%
      mutate(
        kategoria = cut(
          wskaznik,
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE
        )
      )
    
    
    pal <- colorFactor(
      palette = c(
        "#f1f5ff",
        "#d2d9ea",
        "#b3bdd4",
        "#93a1bf",
        "#7485a9",
        "#556994"
      ),
      domain = mapa_data$kategoria,
      na.color = "#f0f0f0"
    )
    
    
    leaflet(
      mapa_data,
      options = leafletOptions(
        dragging = FALSE,
        zoomControl = FALSE,
        scrollWheelZoom = FALSE,
        doubleClickZoom = FALSE,
        boxZoom = FALSE,
        touchZoom = FALSE
      )
    ) %>%
      
      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%
      
      fitBounds(
        lng1 = 14,
        lat1 = 49,
        lng2 = 24.5,
        lat2 = 55
      ) %>%
      
      addPolygons(
        
        layerId = ~JPT_NAZWA_,
        
        fillColor = ~pal(kategoria),
        fillOpacity = 0.9,
        color = "#3a405a",
        weight = 1.5,
        
        popup = ~paste0(
          "<b>Województwo:</b> ",
          str_to_title(JPT_NAZWA_),
          "<br>",
          
          "<b>Sport / 10 000 mieszkańców:</b> ",
          ifelse(is.na(wskaznik), 0, round(wskaznik, 2)),
          "<br>",
          
          if (input$sport == "Wszystkie sporty") {
            paste0(
              "<b>Najpopularniejszy sport:</b> ",
              dyscyplina_sportowa,
              "<br>"
            )
          } else {
            paste0(
              "<b>Liczba osób uprawiających ",
              dyscyplina_sportowa, 
              ": </b>",
              ifelse(is.na(liczba_osob), 0, liczba_osob)
            )
          }
          
        )
      ) %>%
      
      addLegend(
        pal = pal,
        values = ~kategoria,
        title = "Sport / 10 000 mieszkańców",
        position = "bottomright",
        opacity = 1
      ) 
  })
  
  #Ciekawostki
  observeEvent(input$mapa_shape_click, {
    
    klik <- input$mapa_shape_click
    
    output$ciekawostka <- renderText({
      
      woj <- tolower(klik$id)
      
      txt <- ciekawostki[[woj]]
      
      if (is.null(txt)) {
        return("Brak ciekawostki dla tego województwa.")
      }
      
      txt
    })
  })
  
  output$ciekawostka <- renderText({
    
    klik <- input$mapa_shape_click
    
    if (is.null(klik$id)) {
      return("Kliknij województwo na mapie.")
    }
    
    woj <- tolower(trimws(klik$id))
    
    if (!(woj %in% names(ciekawostki))) {
      return("Brak ciekawostki dla tego województwa.")
    }
    
    ciekawostki[[woj]]
  })
  
}


# START

shinyApp(ui, server)