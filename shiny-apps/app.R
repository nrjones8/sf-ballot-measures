library(dplyr)
library(ggplot2)
library(ggthemes)
library(knitr)
library(plotly)
library(shinythemes)

ELECTION_DATE_FORMAT = '%m/%d/%Y'

ui <- shinyUI(
  navbarPage(
    'SF Propositions Historical Explorer',
    tabPanel(
      'Explorer',
      htmlOutput('intro_html'),
      hr(),
      plotlyOutput("prop_results_grouped_by_letter", height = '600px'),
      htmlOutput('prop_details_html'),
      hr(),
      width = 12
    ),
    tabPanel(
      'About',
      hr()
    ),
    # Lots more available: https://rstudio.github.io/shinythemes/
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
  
  measures <- read.csv('data/ballot_measure_history.csv')
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
      # size = pct_margin,
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
      scale_x_discrete('Election Date') +
      ylab('Proposition Letter') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    plt <- ggplotly(g, tooltip = 'text') %>%
      # Thanks https://community.plot.ly/t/disable-interactions-in-plotly-for-r-and-ggplot2/1361
      config(displayModeBar = F) %>%
      # Thank you https://stackoverflow.com/a/38106870
      layout(
        legend = list(
          x = 0.01,
          y = .95,
          orientation = 'h'
        )
        #dragmode = 'select'
        # To add margins:
        # margin = list(r = 100)
        # See https://community.plot.ly/t/move-axis-labels-to-top-right/534/2
        # and https://plot.ly/r/axes/
        # To move the axis to the top, just xaxis = list(side = 'top')
      ) %>%
      add_annotations(
          x = as.numeric(most_popular_prop_u_93$election_date_factor),
          y = as.numeric(most_popular_prop_u_93$prop_letter),
          axref = 'x',
          ayref = 'y',
          # Where should the text actually be displayed?
          ax = as.numeric(most_popular_prop_u_93$election_date_factor) + 8,
          ay = as.numeric(most_popular_prop_u_93$prop_letter) + 2,
          
          showarrow = TRUE,
          text = paste(
            "1993's Prop U, requiring that candidates be SF residents, ",
            "was the most popular of last 30 years, passing with 89% of the vote.",
            sep = "<br />"
          ),
          font = list(
            size = annotation_font_size
          )
      ) %>%
      add_annotations(
        x = as.numeric(airbnb_prop_f_2015$election_date_factor),
        y = as.numeric(airbnb_prop_f_2015$prop_letter),
        
        # https://plot.ly/r/reference/#Layout_and_layout_style_objects because the reference docs don't
        # always show up when googling :'(
        # When we set `ax` and `ay`, make both of those be in the units of the graph, not in pixels
        axref = 'x',
        ayref = 'y',
        # Where should the text actually be displayed?
        ax = as.numeric(airbnb_prop_f_2015$election_date_factor) - 2,
        ay = as.numeric(airbnb_prop_f_2015$prop_letter) + 13,
        showarrow = TRUE,
        text = paste(
          'Stricter regulations on Airbnb fails to pass;',
          'the company <a href="https://www.sfgate.com/bayarea/article/Prop-F-Measure-to-restrict-Airbnb-rentals-6609176.php">put in $8 million</a>',
          'to help defeat the measure.',
          sep='<br />'
        ),
        font = list(
          size = annotation_font_size
        )
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
          ax = as.numeric(feinstein_recall_1983$election_date_factor) + 1,
          ay = as.numeric(feinstein_recall_1983$prop_letter) + 24,
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
    if (length(closest_prop_h_11) > 0) {
      plt <- add_annotations(
        plt,
        x = as.numeric(closest_prop_h_11$election_date_factor),
        y = as.numeric(closest_prop_h_11$prop_letter),
        
        # https://plot.ly/r/reference/#Layout_and_layout_style_objects because the reference docs don't
        # always show up when googling :'(
        # When we set `ax` and `ay`, make both of those be in the units of the graph, not in pixels
        axref = 'x',
        ayref = 'y',
        # Where should the text actually be displayed?
        ax = as.numeric(closest_prop_h_11$election_date_factor),
        ay = as.numeric(closest_prop_h_11$prop_letter) + 17,
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
  
  output$prop_details_html <- renderUI({
    click_event_data <- event_data("plotly_click")
    
    if (!is.null(click_event_data)) {
      clicked_key <- click_event_data$key
      clicked_data <- recent_measure_results %>% filter(key == clicked_key)
      prop_info <- paste(
        'Election Date: ',
        clicked_data$parsed_election_date,
        '<br />',
        'Prop ',
        clicked_data$prop_letter_or_num,
        '<br />',
        '% Yes Votes: ',
        clicked_data$pct_yes_votes,
        sep = ''
      )
      
      header_text <- paste(
        'Prop ', clicked_data$prop_letter_or_num, ' - ', 
        clicked_data$prop_title,
        ' (', clicked_data$election_date, ')',
        sep = ''
      )
      
      passed_or_failed_text <- ifelse(clicked_data$pass_or_fail == 'F', 'This measure failed to pass.', 'This measure passed.')
      outcome_text <- paste(
        'Got ', clicked_data$pct_yes_votes, '% yes votes.',
        passed_or_failed_text
      )
      
      tags$div(class="header", checked=NA,
               tags$h2(header_text),
               tags$h3('Outcome'),
               tags$p(outcome_text),
               tags$h3('Proposition Description'),
               tags$p(clicked_data$description),
               tags$p(
                 'For more information on this proposition, see the',
                 tags$a(href=clicked_data$prop_url, target="_blank", "SF Public Library website."))
      )
      
    } else {
      tags$div(class="header", checked=NA)
    }
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
}

shinyApp(ui, server)