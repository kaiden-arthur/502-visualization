library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(shinyBS)

ds <- read_csv("https://raw.githubusercontent.com/dkunz2024/ds502final/master/pokemon_ds.csv")
ds <- ds %>% 
  mutate(Type_1 = as.factor(Type_1), 
         Type_2 = as.factor(Type_2), 
         Color = as.factor(Color), 
         Egg_Group_1 = as.factor(Egg_Group_1), 
         Egg_Group_2 = as.factor(Egg_Group_2), 
         Body_Style = as.factor(Body_Style))

types <- read_csv("https://raw.githubusercontent.com/dkunz2024/ds502final/master/pokemon_type_ds.csv")
pokemon_types <- types
types <- types %>% 
  rename("Type 1"=...1)
type_list <- types$`Type 1`
types <- types %>% 
  pivot_longer(
    cols=2:19,
    names_to = "Type 2",
    values_to = "Effectiveness"
  )

# Kaiden data 
pokemon_types = pokemon_types %>% 
  pivot_longer(
    cols = Normal:Fairy,
    names_to = "Defending_Type", 
    values_to = "Effectiveness"
  ) %>% 
  rename("Attacking_Type" = ...1) %>% 
  mutate(Effectiveness = cut(Effectiveness, breaks=c(-0.1, 0, 0.4, 0.9, 1.1, 2)))


# for type color extraction 
pokemon_type_names = c("Bug", "Dark", "Dragon", "Electric", "Fairy", "Fighting",
                       "Fire", "Flying", "Ghost", "Grass", "Ground", 
                       "Ice", "Normal", "Poison", "Psychic", "Rock", "Steel",
                       "Water")

pokemon_type_colors = c("#77820f", "#514343", "#232f98", 
                        "#c59b00", "#801e86", "#c66a00", 
                        "#a9220f", "#214a7c", "#5b3a5e", 
                        "#438225", "#73461e", "#3186a6", 
                        "#53585b", "#6a339c", "#9b1e40", 
                        "#545038", "#467285", "#2559b2")


#given a move and two types, reports the effectiveness of using that move 
f.calc_effectiveness <- function(move, type1, type2=NA){
  effectiveness <- 1
  effectiveness = effectiveness * types[types$`Type 1`==move & types$`Type 2`==type1, "Effectiveness"] %>% pull("Effectiveness")
  if(!is.na(type2)){
    effectiveness = effectiveness * types[types$`Type 1`==move & types$`Type 2`==type2, "Effectiveness"] %>% pull("Effectiveness")
  }
  effectiveness
}
#f.calc_effectiveness("Fire", "Water")

#calculates the maximum effectiveness four moves could have against a pokemon
f.max_effectiveness <- function(moves, type1, type2=NA){
  myvec <- numeric(0)
  for (i_move in moves){
    myvec <- c(myvec, f.calc_effectiveness(i_move, type1, type2))
  }
  c(moves[which.max(myvec)], max(myvec))
}

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Pokemon Battle Assistant"),
  
  # Sidebar to select 2 pokemon  
  sidebarLayout(
    sidebarPanel(
      selectInput("p1", 
                  h4("Attacking Pokemon"),
                  choices = unique(ds$Name), 
                  selected="Pikachu"),
      ## sub-sidebar for move types 
      bsCollapse(
        id = "move_types_panel",
        open = "Panel 1",  # Initial open panel
        bsCollapsePanel(
          title = "Move Types",
          selectInput("move1", "", type_list, selected = "Normal"),
          selectInput("move2", "", type_list, selected = "Normal"),
          selectInput("move3", "", type_list, selected = "Normal"),
          selectInput("move4", "", type_list, selected = "Normal"),
          style = "primary"
        )
      ),
      selectInput("p2", 
                  h4("Defending Pokemon"), 
                  choices = unique(ds$Name),
                  selected = "Charmander")
    ),
    
    # Show the plots
    mainPanel(
      plotOutput("radarPlot"), 
      plotOutput("typesPlot"),
      br(),
      textOutput("typeFacts")
    )
  )
)

# Define server 
server <- function(input, output) {
  
  # stats plot
  output$radarPlot <- renderPlot({
    
    p1 <- input$p1
    p2 <- input$p2
    
    #Getting type colors
    p1_type <-  ds %>% 
      filter(Name==p1) %>% 
      pull(Type_1)
    p2_type <-  ds %>% 
      filter(Name==p2) %>% 
      pull(Type_1)
    p1_color <- pokemon_type_colors[which(pokemon_type_names == p1_type)]
    p2_color <- pokemon_type_colors[which(pokemon_type_names == p2_type)]
    
    #Create dataset of the two pokemon
    dsc <- ds %>%
      filter(Name==p1 | Name==p2) %>%
      mutate(Number=(Name==p2)+1) %>%
      select(Number, HP, Attack, Defense, Sp_Atk, Sp_Def, Speed) %>%
      pivot_longer(
        cols=2:7,
        names_to = "Stat_Type",
        values_to = "Stat_Value"
      ) %>%
      mutate(Stat_Type=factor(Stat_Type, levels=c("HP", "Attack", "Defense", "Speed", "Sp_Def", "Sp_Atk"))) %>%
      arrange(Number) %>% 
      mutate(Number = as.factor(Number))
    
    #Create the plot
    dsc %>%
      ggplot(aes(x=Stat_Type, y=Stat_Value, fill=Number))+
      geom_col()+
      scale_fill_manual(values=c(p1_color, p2_color)) + 
      facet_wrap(.~Number, labeller=as_labeller(c('1'=p1, '2'=p2)))+
      coord_polar(start=-0.5, theta="x")+
      scale_y_continuous(limits=function(limits){
        c(0, limits[2])
      })+
      labs(x="", y="", title="Stat Differences", fill=paste(p1, ">", p2),
           caption="Dataset from Kaggle: https://www.kaggle.com/datasets/alopez247/pokemon")+
      theme_minimal()+
      theme(
        strip.text.x = element_text(size=10, face="bold"), 
        legend.position = "none"
      )
  })
  
  # type chart 
  output$typesPlot <- renderPlot({
    
    # define variables from inputs
    if (is.null(input$move_types_panel)){
      attackingType <- ds %>% 
        filter(Name==input$p1) %>% 
        pull(Type_1) %>% 
        as.character()
      defendingType <- c(
        as.character(ds[ds$Name==input$p2, "Type_1"] %>% pull("Type_1")),
        as.character(ds[ds$Name==input$p2, "Type_2"] %>% pull("Type_2"))
      )
    } else {
      attackingType <- c(input$move1, input$move2, input$move3, input$move4)
      defendingType <- c(
        as.character(ds[ds$Name==input$p2, "Type_1"] %>% pull("Type_1")),
        as.character(ds[ds$Name==input$p2, "Type_2"] %>% pull("Type_2"))
      )
    }
    
    # filter
    attacking = pokemon_types %>%
      filter(Attacking_Type %in% attackingType)
    defending = pokemon_types %>%
      filter(Defending_Type %in% defendingType)
    both_types = pokemon_types %>%
      filter(Attacking_Type %in% attackingType) %>%
      filter(Defending_Type %in% defendingType)
    
    # the plot 
    p = ggplot(pokemon_types) +
      geom_tile(aes(x=Attacking_Type, y=Defending_Type, fill=Effectiveness), color="black", alpha=0.3) +
      geom_tile(data=attacking, aes(x=Attacking_Type, y=Defending_Type, fill=Effectiveness), color="black", linewidth=0.5, alpha=0.5) +
      geom_tile(data=defending, aes(x=Attacking_Type, y=Defending_Type, fill=Effectiveness), color="black", linewidth=0.5, alpha=0.5) +
      geom_tile(data=both_types, aes(x=Attacking_Type, y=Defending_Type, fill=Effectiveness), color="black", linewidth=0.5) +
      scale_fill_manual(values=c("black", "red","white", "green"), 
                        labels=c("No Effect \n (0x)", "Not very effective \n (0.5x)", "Normal \n (1x)", "Super effective \n (2x)"))+
      labs(
        x = "Attacking Type",
        y = "Defending Type", 
        title = "Type Effectiveness"
      ) +
      theme(axis.text.x = element_text(size=8), # legible axis labels (at least not overlapping)
            axis.text.y = element_text(size=8) 
      ) +
      scale_x_discrete(position = "top") 
    
    p
    
  })
  
  # reactive text: type- or move-based 
  output$typeFacts <- renderText({
    # if types:
    if (is.null(input$move_types_panel)){
      #get pokemon types
      attackingType <- as.character(ds[ds$Name==input$p1, "Type_1"] %>% pull("Type_1"))
      defendingType <- c(
        as.character(ds[ds$Name==input$p2, "Type_1"] %>% pull("Type_1")),
        as.character(ds[ds$Name==input$p2, "Type_2"] %>% pull("Type_2"))
      )
      paste(attackingType, " is ", f.calc_effectiveness(attackingType, defendingType[1], defendingType[2]), 
            "x effective against ", input$p2, ".", sep="")
      # if moves:
    } else {
      attackingType <- c(input$move1, input$move2, input$move3, input$move4)
      defendingType <- c(
        as.character(ds[ds$Name==input$p2, "Type_1"] %>% pull("Type_1")),
        as.character(ds[ds$Name==input$p2, "Type_2"] %>% pull("Type_2"))
      )
      results <- f.max_effectiveness(attackingType, defendingType[1], defendingType[2])
      paste(results[1], " will be your best move against ", input$p2, " with ", results[2], "x effectiveness.", sep="")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)