"%OR%" <- function(a, b) if (!is.null(a)) a else b

sidebarMenu <- function(..., id = NULL, title = NULL) {
  
  items <- list(...)
  
  div(
    id = "sidebar-menu",
    class = "main_menu_side hidden-print main_menu",
    div(
      class = "menu_section",
      tags$h3(title),
      tags$ul(
        class = "nav side-menu",
        tagList(lapply(items, tags$li))
      )
    )
  )
}

ivalueBox <- function(value, title = NULL, description = NULL, icon = NULL, width = 3) {
  div(
    class = paste0("animated flipInY col-lg-3 col-md-", width," col-sm-6 col-xs-12"),
    div(
      class = "tile-stats",
      div(
        class = "icon0",
        icon
      ),
      div(class = "count", value),
      h3(title),
      p(description)
    )
  )
}

descriptionBlock <- function(number = NULL, number_color = NULL, number_icon = NULL,
                             description_icon = NULL, header = NULL, text = NULL, 
                             text_comparison = NULL,
                             right_border = TRUE,
                             margin_bottom = FALSE,
                             customcss = FALSE) {
  
  cl <- "description-block"
  if (isTRUE(right_border)) cl <- paste0(cl, " border-right")
  if (isTRUE(margin_bottom)) cl <- paste0(cl, " margin-bottom")
  
  numcl <- "description-percentage"
  if (!is.null(number_color)) numcl <- paste0(numcl, " text-", number_color)
  
  shiny::tags$div(
    class = cl,
    if (!customcss) {
      shiny::tags$span(class = "description-text", 
                       if (!is.null(description_icon)) shiny::tags$i(class = description_icon),
                       text)
    } else {
      
      shiny::tags$span(class = "mydescription-text", 
                       if (!is.null(description_icon)) shiny::tags$i(class = description_icon),
                       text)
    },
    
    if (!customcss) {
      shiny::tags$h5(class = "description-header", header)
    } else {
      shiny::tags$h5(class = "mydescription-header", header)
    }
    
    ,
    shiny::tags$span(
      class = numcl, 
      if (!is.null(number_icon)) shiny::tags$i(class = number_icon),
      number
    ),
    shiny::tags$span(class="mycss", text_comparison)
  )
}

Column2 <- function (..., width = NULL, center = FALSE, offset = NULL) {
  colCl <- "col-sm"
  if (!is.null(width)) colCl <- paste0(" col-sm-", width)
  if (!is.null(offset)) colCl <- paste0(colCl, " offset-sm-", offset)
  if (center) colCl <- paste0(colCl, " text-center")
  htmltools::tags$div(class = colCl, ...)
}

InfoCard <- function(value, title = NULL, stat = NULL, stat_icon = NULL,
                     stat_color = NULL,
                     description = NULL, icon, icon_background = "default", 
                     hover_lift = FALSE, shadow = FALSE, 
                     background_color = NULL, gradient = FALSE, width = 3) {
  
  iconCl <- "icon-shape text-white rounded-circle shadow"
  numcl <- "description-percentage"
  if (!is.null(stat_color)) numcl <- paste0(numcl, " text-", stat_color)
  
  if (!is.null(icon_background)) iconCl <- paste0(iconCl, " bg-", icon_background)
  
  cardCl <- "card card-stats mb-4"
  if (hover_lift) cardCl <- paste0(cardCl, " card-lift--hover")
  if (shadow) cardCl <- paste0(cardCl, " shadow")
  if (gradient) {
    if (!is.null(background_color)) cardCl <- paste0(cardCl, " bg-gradient-", background_color)
  } else {
    if (!is.null(background_color)) cardCl <- paste0(cardCl, " bg-", background_color)
  }
  
  if (!is.null(background_color))
    if (background_color == "default") text_color <- "text-white" else text_color <- NULL
  else text_color <- NULL
  
  infoCardTag <- shiny::tags$div(
    class = cardCl,
    shiny::tags$div(
      class = "card-body",
      # upper part
      shiny::fluidRow(
        Column2(
          shiny::tags$h5(class = paste0("card-title text-uppercase2 mb-2", text_color), title),
          shiny::span(class = paste0("h2 font-weight-bold mb-1", text_color), value)
        ),
        shiny::tags$div(
          class = "col-auto",
          shiny::tags$div(
            class = iconCl,
            icon
          )
        )
      ),
      # lower part
      shiny::tags$span(
        class = numcl, 
        if (!is.null(stat_icon)) shiny::tags$i(class = stat_icon),
        stat
      ),
      shiny::tags$span(class="mycss0", description)
    )
  )
  
  Column2(width = width, infoCardTag)
  
}

d_InfoCard <- function(value, title = NULL, stat = NULL, stat_icon = NULL,
                       stat_color = NULL,
                       description = NULL, icon, icon_background = "default", 
                       hover_lift = FALSE, shadow = FALSE, 
                       background_color = NULL, gradient = FALSE, width = 3,
                       customcss = FALSE) {
  
  iconCl <- "icon2 icon2-shape text-white rounded-circle shadow"
  numcl <- "description-percentage"
  if (!is.null(stat_color)) numcl <- paste0(numcl, " text-", stat_color)
  
  if (!is.null(icon_background)) iconCl <- paste0(iconCl, " bg-", icon_background)
  
  cardCl <- "card card2-stats mb-4"
  if (hover_lift) cardCl <- paste0(cardCl, " card-lift--hover")
  if (shadow) cardCl <- paste0(cardCl, " shadow")
  if (gradient) {
    if (!is.null(background_color)) cardCl <- paste0(cardCl, " bg-gradient-", background_color)
  } else {
    if (!is.null(background_color)) cardCl <- paste0(cardCl, " bg-", background_color)
  }
  
  if (!is.null(background_color))
    if (background_color == "default") text_color <- "text-white" else text_color <- NULL
  else text_color <- NULL
  
  shiny::tags$div(
    class = cardCl,
    shiny::tags$div(
      class = "card-body",
      # upper part
      shiny::fluidRow(
        argonColumn(
          
          if (!customcss) {
            shiny::tags$h5(class = paste0("card-title mb-1", text_color), title)
          } else {
            shiny::tags$h5(class = paste0("card-title2 mb-1", text_color), title)
          }
          
          ,
          
          if (!customcss) {
            shiny::span(class = paste0("h2 font-weight-bold", text_color), value)
          } else {
            shiny::span(class = "mydescription-header2", value)
          }
        )
        ,
        shiny::tags$div(
          class = "col-auto",
          shiny::tags$div(
            class = iconCl,
            icon
          )
        )
      ),
      # lower part
      shiny::tags$span(
        class = numcl, 
        if (!is.null(stat_icon)) shiny::tags$i(class = stat_icon),
        stat
      ),
      shiny::tags$span(class="mycss0", description)
    )
  )
  
}
