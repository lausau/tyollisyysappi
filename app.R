library(shiny)
Sys.setlocale(locale="UTF-8") 

# PXWEB query 
pxweb_query_list <- 
    list("Alue"=c("SSS","MK01","MK02","MK04","MK05","MK06","MK07","MK08","MK09","MK10","MK11","MK12","MK13","MK14","MK15","MK16","MK17","MK18","MK19","MK21"),
         "Pääasiallinen toiminta"=c("11"),
         "Koulutusaste"=c("SSS","3_4","5_6","7_8","9_X"),
         "Tiedot"=c("vaesto"),
         "Sukupuoli"=c("1","2"),
         "Ikä"=c("SSS"),
         "Vuosi"=c("1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))

# Download data 
px_data <- 
    pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/vrm/tyokay/statfin_tyokay_pxt_115d.px",
              query = pxweb_query_list)

# Convert to data.frame 
px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

## Shinyapp jossa käyttäjä voi valita maakunnan ja sukupuolen, ja nähdä työllisten määrän per koulutus
# Mitä arvoja Alue-muuttuja saa
unique(px_data$Alue)

data <- px_data %>% filter(Alue!="KOKO MAA") %>% filter(Koulutusaste!="Yhteensä")
data <- data %>% rename("vaesto" = "Väestö 31.12.") %>% rename("ika" = "Ikä")
data <- data %>% select(-c("Pääasiallinen toiminta", "ika"))
# poistetaan MK00-tyyppinen alku maakuntien nimistä
data$Alue <- gsub("(^MK[0-9][0-9] )", "", data$Alue)
# poistetaan 1,2 -tyyppinen loppu koulutusten nimistä
data$Koulutusaste <- gsub("([0-9], [0-9]$)", "", data$Koulutusaste)
# Uudelleenjärjestetään koulutusasteiden luokat kuviota ja ehkä myöhempää käyttöä varten
# tarkistetaan luokkien nimet 
unique(data$Koulutusaste)
data$Koulutusaste <- factor(data$Koulutusaste, 
    levels=c("Ei perusasteen jälkeistä tutkintoa tai koulutusaste tuntematon",
             "Toinen aste tai erikoisammattikoulutusaste ",
             "Alin korkea-aste tai alempi korkeakouluaste ",
             "Ylempi korkeakouluaste tai tutkijakoulutusaste "),
    labels=c("Ei perusasteen jälkeistä tutkintoa tai koulutusaste tuntematon",
             "Toinen aste tai erikoisammattikoulutusaste ",
             "Alin korkea-aste tai alempi korkeakouluaste ",
             "Ylempi korkeakouluaste tai tutkijakoulutusaste "))

# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$style("#cite {
               font-size:8px;
               text-align:center;
               padding-top:10px;"),
    
    # Application title
    titlePanel(h1("Työllisten määrä koulutusasteen mukaan naisilla ja miehillä 1987-2019",h3("Valitse valikosta haluasi maakunta"))),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Alue",
                        label = "Valitse maakunta",
                        choices = list("Ahvenanmaa","Etelä-Karjala",
                                       "Etelä-Pohjanmaa","Etelä-Savo","Kainuu",           
                                       "Kanta-Häme","Keski-Pohjanmaa", "Keski-Suomi",
                                       "Kymenlaakso","Lappi","Päijät-Häme","Pirkanmaa",
                                       "Pohjanmaa","Pohjois-Karjala","Pohjois-Pohjanmaa",
                                       "Pohjois-Savo","Satakunta","Uusimaa","Varsinais-Suomi"),
                        selected = "vaesto")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("cite")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        df3 <- filter(data, Alue==input$Alue)
        ggplot(df3,aes(y = vaesto,x = Vuosi, group=Koulutusaste)) +
            geom_line(aes(color=Koulutusaste)) +
            facet_wrap(~Sukupuoli) +
            scale_x_discrete(breaks = seq.int(from=1987, to=2019, by=5)) +
            labs(y="Väestö", x="Vuosi") +
            theme_light() +
            theme(legend.position="bottom") +
            guides(col=guide_legend(ncol=1,byrow=TRUE)) 
    })
        output$cite <- renderText({ 
            ("(C) Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti
            (rOpenGov 2014-2016).  pxweb: R tools for PXWEB API.  URL:
                http://github.com/ropengov/pxweb")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


