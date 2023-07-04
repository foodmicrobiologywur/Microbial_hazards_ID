
## define info button function for tables
# define the question button in a button since we need to uses multiple times
infoBtn <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question"),
               style = "info",
               size = "extra-small",
               class='btn action-button btn-info btn-xs shiny-bound-input'
  )
}
