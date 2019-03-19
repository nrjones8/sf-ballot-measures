library(dplyr)
library(ggplot2)
library(ggthemes)
library(knitr)
library(plotly)
library(shinythemes)

ELECTION_DATE_FORMAT = '%m/%d/%Y'
PLOTLY_SOURCE_PLT_NAME <- 'main_props_plot'

ui <- shinyUI(
  navbarPage(
    'SF Propositions Historical Explorer',
    tabPanel(
      'Explorer',
      htmlOutput('intro_html'),
      hr(),
      fluidRow(
        column(
          # Width of container is 8, so make the plot itself fill that entire container
          width = 8, plotlyOutput("prop_results_grouped_by_letter", width = "100%", height = '1200px')
        ),
        column(width = 3, htmlOutput('prop_details_html'))
      ),
      hr(),
      width = 12
    ),
    tabPanel(
      'About',
      hr()
    ),
    header = tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'props.css')
    ),
    # Lots more available: https://rstudio.github.io/shinythemes/
    # https://bootswatch.com/flatly/
    theme = shinytheme('flatly')
  )
)


server <- function(input, output) {
  annotation_font_size <- 10
  # Last 30 years
  earliest_year <- 1980
  latest_year <- 2018
  
  earliest_year_date <- as.Date(paste('1/1/', earliest_year, sep=''), ELECTION_DATE_FORMAT)
  latest_year_date <- as.Date(paste('12/31/', latest_year, sep=''), ELECTION_DATE_FORMAT)
  
  measures <- read.csv('../data/ballot_measure_history.csv')
  measures$parsed_election_date <- as.Date(measures$election_date, ELECTION_DATE_FORMAT)
  
  # Make a readable `Outcome` variable, we'll plot that instead of the raw `pass_or_fail` field
  measures$Outcome <- ifelse(measures$pass_or_fail == 'P', 'Passed', 'Failed')
  recent_measure_results <- measures %>% 
    filter(parsed_election_date > earliest_year_date & parsed_election_date < latest_year_date)
  
  # This is confusing / bizarre, but https://gist.github.com/daattali/9440f0b278dbbf538b3587e026811426#gistcomment-2207525
  # was the inspiration and sort of helps explain
  recent_measure_results$key <- row.names(recent_measure_results)
  
  output$prop_results_grouped_by_letter <- renderPlotly({
    # To change the plot to start from top down, just `rev` this factor
    sorted_prop_factor <- c(
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
      'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'AA', 'BB', 'RR',
      # prop 2 was a regional measure, maybe just remove it?
      '2'
    )
    recent_measure_results$prop_letter <- factor(recent_measure_results$prop_letter_or_num, sorted_prop_factor)
    recent_measure_results$pct_margin <- as.numeric(as.character(recent_measure_results$pct_yes_votes)) - as.numeric(as.character(recent_measure_results$pct_required_to_pass))
    
    # Make date a factor - if it's a date, the spacing looks funny because there's no uniform amount of time
    # between elections
    recent_measure_results$election_date_factor <- as.factor(format(recent_measure_results$parsed_election_date, format = "%Y/%m"))
    
    # These were grabbed manually; we could instead sort the DF by most/least popular on the fly here, but that
    # makes writing proposition-specific description really hard. So they're hard-coded, which I think is a
    # reasonable tradeoff.
    most_popular_prop_u_93 <- recent_measure_results %>% 
      filter(election_year == '1993' & election_month == '11' & prop_letter_or_num == 'U')
    
    least_popular_prop_g_15 <- recent_measure_results %>% 
      filter(election_year == '2015' & election_month == '11' & prop_letter_or_num == 'G')
    
    closest_prop_h_11 <- recent_measure_results %>% 
      filter(election_year == '2011' & election_month == '11' & prop_letter_or_num == 'H')
    
    airbnb_prop_f_2015 <- recent_measure_results %>% 
      filter(election_year == '2015' & election_month == '11' & prop_letter_or_num == 'F')
    
    # TODO ADD FEINSTEIN and add http://sfballotprops.com/1983-an-odd-year-of-props.html
    feinstein_recall_1983 <- recent_measure_results %>% 
      filter(election_year == '1983' & election_month == '4' & prop_letter_or_num == 'A')

    g <- ggplot(recent_measure_results, aes(
      key = key,
      x = election_date_factor, 
      y = prop_letter, 
      color = Outcome,
      # size = ifelse(pass_or_fail == 'P', pct_yes_votes, pct_no_votes),
      size = pct_yes_votes,
      text = paste(
        'Election Date: ',
        parsed_election_date,
        '<br />',
        'Prop ',
        prop_letter_or_num,
        '<br />',
        '% Yes Votes: ',
        pct_yes_votes,
        '<br />',
        Outcome,
        '<br />',
        '(Click to show more details below plot)',
        sep = ''
      )
    )) +
      geom_point() +
      scale_size(range = c(.3, 3.5)) +
      guides(size = guide_legend()) +
      scale_x_discrete('Election Date', position = 'top') +
      ylab('Proposition Letter') +
      coord_flip()

    plt <- ggplotly(g, tooltip = 'text', source = PLOTLY_SOURCE_PLT_NAME) %>%
      # Thanks https://community.plot.ly/t/disable-interactions-in-plotly-for-r-and-ggplot2/1361
      config(displayModeBar = F) %>%
      # Thank you https://stackoverflow.com/a/38106870
      layout(
        legend = list(
          x = .8,
          y = .95,
          orientation = 'v'
        ),
        # See https://community.plot.ly/t/move-axis-labels-to-top-right/534/2
        # and https://plot.ly/r/axes/
        xaxis = list(side = 'top'),
        # TODO get the xaxis to show on the bottom as well
        # this doesn't actually seem to do anything :'(
        dragmode = FALSE
      )
      if (length(feinstein_recall_1983) > 0) {
        plt <- add_annotations(
          plt,
          x = as.numeric(feinstein_recall_1983$election_date_factor),
          y = as.numeric(feinstein_recall_1983$prop_letter),

          # https://plot.ly/r/reference/#Layout_and_layout_style_objects because the reference docs don't
          # always show up when googling :'(
          # When we set `ax` and `ay`, make both of those be in the units of the graph, not in pixels
          axref = 'x',
          ayref = 'y',
          # Where should the text actually be displayed?
          ax = as.numeric(feinstein_recall_1983$election_date_factor) + 14,
          ay = as.numeric(feinstein_recall_1983$prop_letter) + 3,
          showarrow = TRUE,
          text = paste(
            'Mayor Feinstein <a href="http://sfballotprops.com/1983-an-odd-year-of-props.html">faces a recall election over</a>',
            'her leadership in banning handguns.',
            'Over 80% voted to keep her in office.',
            sep='<br />'
          ),
          font = list(
            size = annotation_font_size
          )
        )
      }
    if (length(airbnb_prop_f_2015) > 0) {
      plt <- add_annotations(
        plt,
        x = as.numeric(airbnb_prop_f_2015$prop_letter),
        y = as.numeric(airbnb_prop_f_2015$election_date_factor),

        # https://plot.ly/r/reference/#Layout_and_layout_style_objects because the reference docs don't
        # always show up when googling :'(
        # When we set `ax` and `ay`, make both of those be in the units of the graph, not in pixels
        axref = 'x',
        ayref = 'y',
        # Where should the text actually be displayed?
        ax = as.numeric(airbnb_prop_f_2015$prop_letter) + 11,
        ay = as.numeric(airbnb_prop_f_2015$election_date_factor) + 1,
        showarrow = TRUE,
        text = paste(
          'Stricter regulations on Airbnb fails to pass; the company',
          '<a href="https://www.sfgate.com/bayarea/article/Prop-F-Measure-to-restrict-Airbnb-rentals-6609176.php">put in $8 million</a> to help defeat the measure.',
          sep='<br />'
        ),
        font = list(
          size = annotation_font_size
        )
      )
    }

    if (length(closest_prop_h_11) > 0) {
      plt <- add_annotations(
        plt,
        x = as.numeric(closest_prop_h_11$prop_letter),
        y = as.numeric(closest_prop_h_11$election_date_factor),
        
        # https://plot.ly/r/reference/#Layout_and_layout_style_objects because the reference docs don't
        # always show up when googling :'(
        # When we set `ax` and `ay`, make both of those be in the units of the graph, not in pixels
        axref = 'x',
        ayref = 'y',
        # Where should the text actually be displayed?
        ax = as.numeric(closest_prop_h_11$prop_letter) + 8,
        ay = as.numeric(closest_prop_h_11$election_date_factor) + 2,
        showarrow = TRUE,
        # (91678 no + 91525 yes) = 183,203 total --> 91602 required to pass.
        # 91602 - 91525 = 77 votes short of passing
        text = paste(
          'Prop H in 2011 was the closest vote in the last 30 years; it fell short by',
          'just 77 votes out of 183,203 votes cast.',
          'Read more about it <a href="http://www.sfexaminer.com/proposition-h-addresses-san-francisco-school-choice-policy/">here</a>',
          sep='<br />'
        ),
        font = list(
          size = annotation_font_size
        )
      )
    }
    # https://github.com/ropensci/plotly/issues/842#issuecomment-349889093
    #hide_legend(plt)
  })

  observeEvent(event_data("plotly_click", source = PLOTLY_SOURCE_PLT_NAME), {
    # The `source` argument in `event_data` tells plotly which plot to look for. "Match the value of this string
    # with the source argument in plot_ly() to retrieve the event data corresponding to a specific plot"
    click_event_data <- event_data("plotly_click", source = PLOTLY_SOURCE_PLT_NAME)
    clicked_key <- click_event_data$key

    to_display <- recent_measure_results %>% filter(key == clicked_key)
    output$prop_details_html <- renderUI({ display_prop_details(to_display) })
  })
  
  output$intro_html <- renderUI({
    tags$div(
      tags$p(paste(
        'In the past 20 years, San Francisco voters have voted on over 300 ballot propositions. ',
        'Some measures pass with overwhelming support - such as Prop T from November of 2016, which ',
        'restricts gifts and campaign contributions from lobbyists. ',
        'Others are much closer, failing or passing by just a few hundred votes - such as Prop H from November of 2011, which ',
        'would have given the highest priority to assigning each student to the school closest to the student\'s home. ',
        sep = ''
      )),
      tags$p(paste(
        'Below, you can explore the outcomes of historical ballot measures in San Francisco. The color of the point indicates ',
        'whether the measure passed; larger points correspond to greater support or opposition, while smaller points ',
        'are used for measures that barely passed or barely failed. Clicking on an individual ',
        'point on the graph will display more details of the measure below the graph.'
      ))
    )
  })
  
  display_prop_details <- function(prop_to_show) {
    # displays data from the `prop_to_show` row. It's a miracle this works?
    prop_info <- paste(
      'Election Date: ',
      prop_to_show$parsed_election_date,
      '<br />',
      'Prop ',
      prop_to_show$prop_letter_or_num,
      '<br />',
      '% Yes Votes: ',
      prop_to_show$pct_yes_votes,
      sep = ''
    )
    
    header_text <- paste(
      'Prop ', prop_to_show$prop_letter_or_num, ' - ', 
      prop_to_show$prop_title,
      ' (', prop_to_show$election_date, ')',
      sep = ''
    )
    
    passed_or_failed_text <- ifelse(prop_to_show$pass_or_fail == 'F', 'This measure failed to pass.', 'This measure passed.')
    outcome_text <- paste(
      'Got ', prop_to_show$pct_yes_votes, '% yes votes.',
      passed_or_failed_text
    )

    tags$div(class="header", checked=NA,
             tags$h3(header_text),
             tags$h4('Outcome'),
             tags$p(outcome_text),
             tags$h4('Proposition Description'),
             tags$p(prop_to_show$description),
             tags$p(
               'For more information on this proposition, see the',
               tags$a(href=prop_to_show$prop_url, target="_blank", "SF Public Library website."))
    )
  }
}

shinyApp(ui, server)