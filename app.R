# Shiny App to visualize and predict Google Cloud Compute Bucket pricing
# Axel Thieffry - August 2020

library(shiny)
library(tidyverse)
library(magrittr)
library(reshape2)
library(shinythemes)

########## UI ##########
########################

ui <- fluidPage(theme=shinytheme('united'),
    # title
    titlePanel('Google Cloud Platform - Bucket pricing'),
    h4('Based on Regional location'),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=3,
                     selectInput(inputId='region', label='Region', choices=list('Finland',
                                                                                'Belgium',
                                                                                'Netherlands',
                                                                                'London',
                                                                                'Frankfurt',
                                                                                'Zurich'), selected='Finland', multiple=F),
                     radioButtons(inputId='view', label='View', choices=list('Action', 'Bucket', 'Heatmap'), selected='Action'),
                     radioButtons(inputId='currency', label='Currency', choices=list('USD', 'DKK'), selected='USD', inline=T),
                     sliderInput(inputId='months', label='Months:', min=1, max=36, value=1, step=1),
                     sliderInput(inputId='storage_vol', label='Storage volume (GBs):', min=0, max=5000, value=1, step=250),
                     sliderInput(inputId='retrieval_vol', label='Retrieval volume (GBs):', min=0, max=5000, value=1, step=50),
                     sliderInput(inputId='classA_ops', label='Class A operations (x1000):', min=1, max=25, value=1, step=1),
                     sliderInput(inputId='classB_ops', label='Class B operations (x1000):', min=1, max=25, value=1, step=1),
                     br(), 'Axel Thieffry', br(), 'August 2020 (v1.1)'),
    # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                    tabPanel(title='GCP Buckets',
                             tags$h4('Introduction'),
                             'This app estimates the costs of Google Cloud Storage solutions (buckets) according to the main price deteminants such as 
                             (i) the hosting region, (ii) the bucket type, (iii) the storage volume, and (iv) the usage time. Prices are available in USD and can be converted into DKK. 
                             Nominal USD prices were manually gathered from the Bucket creation page on the Google Cloud Console. While the main concepts linked to buckets and their pricing are 
                             recapitulated here, consulting of the ', tags$a(href='https://cloud.google.com/storage/docs', 'full official documentation'), ' is highly recommended.', br(),
                             hr(),
                             tags$h4('Basic bucket price determinants'),
                             tags$b('Region'), br(),
                             'As opposed to ', tags$em('Multi-Region'), ' and ', tags$em('Dual-Region'), ', ', tags$em('Regional'),' buckets are hosted only in one location. 
                             This app focuses solely on bucket pricing for the Regional location amongst European countries. The full list of regions is available ', tags$a(href='https://cloud.google.com/storage/docs/locations#location-r', 'here'), '.', br(), br(),
                             tags$b('Bucket classes'), br(),
                             tableOutput('bucket_class'),
                             tags$b('Storage volume'), br(),
                             'Naturally, data stored in a bucket incur charges based on the bucket class and the volume of data. 
                             Bucket classes intended for increased access have a higher per-Gb storage pricing. 
                             Note that the total bucket size constitutes the basis for cost calculation, independently of the proportion of occupied space in that bucket. ', br(), br(),
                             tags$b('Retrieval'), br(),
                             'There are costs associated with retrieving data or metadata, including reading, copying, or rewriting. 
                             Those only apply for Nearline, Coldline, and Archive classes which are intended for infrequently accessed data. 
                             Only the retried monthly volume is charged, not the entier bucket capacity.', br(), br(),
                             tags$b('Network usage (egress)'), br(),
                             'Network egress costs apply when moving data between buckets or when another Google Cloud service access data in the bucket. 
                             Nevertheless, those are FREE when data moves within the same location (region). This app does not assumes costs projection based on network usage. 
                             Read more ', tags$a(href='https://cloud.google.com/storage/pricing#network-buckets', 'here.'), br(), br(),
                             tags$b('Operations'), br(),
                             'Operations (ops) are actions that modify or retrieve information about buckets and objects in Cloud Storage. There are two types of operations:', br(),
                             '> Class A ops: Adding objects (aka files), list of bucket, listing of objects.', br(),
                             '> Class B ops: Objet GETs, retrieving bucket and object metadata.', br(),
                             tags$a(href='https://cloud.google.com/storage/pricing#operations-by-class', 'More information on operations.')
                             ),
                    tabPanel(title='Nominal Pricing',
                             h4('At-rest nominal princing'),
                             plotOutput('nominal_pricing_plot'),
                             tableOutput('nominal_pricing_table')
                             ),
                    tabPanel(title='Cost Projection',
                             h4('At-rest cost projection'),
                             plotOutput('priced_plot'),
                             tableOutput('table_prices'),
                             '*Those projections do not include minimum duration.'
                             )
                        )
                  )
                )
    )


########### SERVER ###########
##############################
server <- function(input, output) {
    
    # S0. Bucket classes (first tab)
    output$bucket_class <- renderTable({ tibble('class'=c('STANDARD', 'NEARLINE', 'COLDLINE', 'ARCHIVE'),
                                                `Min. duration`=c('/', '30 days', '90 days', '365 days'),
                                                `Typical access type`=c('frequent', '1x / month', '1x / quarter', 'very rare'),
                                                `Recommended storage time`=c('short', 'mid', 'longer', 'longest'),
                                                ) %>%
                                         as.data.frame() %>%
                                         column_to_rownames('class')
                                      }, striped=T, bordered=T, hover=T, spacing='xs', width='100%', align='c', rownames=T)
        
    # S1. make heatmap color palette
    heatmap_colors <- colorRampPalette(colors=c('white', 'navy', 'red'))

    # S2. select data frame with nominal pricing according to selected region
    # data gathered from Google "Cloud Console > Storage > Create a Bucket" (Regional storage type)
    bucket_levels <- factor(c('STANDARD', 'NEARLINE', 'COLDLINE', 'ARCHIVE'))
    
    bucket_nominal_df <- reactive({ # prices for: Finland (eu-north1), Belgium (eu-west1), Netherlands (eu-west4)
                                    if (input$region %in% c('Finland', 'Belgium', 'Netherlands')) { data.frame('Bucket'=bucket_levels,
                                                                                                               'Storage'=c(0.02, 0.01, 0.004, 0.0012),
                                                                                                               'Retrieval'=c(0, 0.01, 0.02, 0.05),
                                                                                                               'Class_A'=c(0.005, 0.01, 0.01, 0.05),     # for 1000 operations
                                                                                                               'Class_B'=c(0.0004, 0.001, 0.005, 0.05))  # for 1000 operations
                                    } else if (input$region == 'London') { data.frame('Bucket'=bucket_levels, # prices for: London (eu-west2)
                                                                                      'Storage'=c(0.023, 0.013, 0.007, 0.0025),
                                                                                      'Retrieval'=c(0, 0.01, 0.02, 0.05),
                                                                                      'Class_A'=c(0.005, 0.01, 0.01, 0.05),
                                                                                      'Class_B'=c(0.0004, 0.001, 0.005, 0.05))
                                    } else if (input$region == 'Frankfurt') { data.frame('Bucket'=bucket_levels, # Frankfurt (eu-west3)
                                                                              'Storage'=c(0.023, 0.013, 0.006, 0.0025),
                                                                              'Retrieval'=c(0, 0.01, 0.02, 0.05),
                                                                              'Class_A'=c(0.005, 0.01, 0.01, 0.05),
                                                                              'Class_B'=c(0.0004, 0.001, 0.005, 0.05))
                                    } else if (input$region == 'Zurich') { data.frame('Bucket'=bucket_levels, # Zurich (eu-west6)
                                                                                      'Storage'=c(0.025, 0.014, 0.007, 0.0025),
                                                                                      'Retrieval'=c(0, 0.01, 0.02, 0.05),
                                                                                      'Class_A'=c(0.005, 0.01, 0.01, 0.05),
                                                                                      'Class_B'=c(0.0004, 0.001, 0.005, 0.05))
                                    }
                                 })

    # S3. Nominal Pricing PLOT
    output$nominal_pricing_plot <- renderPlot({
                                                df <- bucket_nominal_df() %>%
                                                      melt(id.vars='Bucket', variable.name='Action', value.name='USD') %>%
                                                      mutate('Bucket'=factor(Bucket, levels=bucket_levels)) %>%
                                                      { if (input$currency == 'DKK') mutate(., 'USD'=USD*6.34) else . }
                                                
                                                if (input$view == 'Action') {ggplot(df, aes(x=Action, y=USD, fill=Bucket)) +
                                                                                    geom_bar(stat='identity', position=position_dodge(), lwd=.3, col='black', alpha=.6) +
                                                                                    cowplot::theme_cowplot() +
                                                                                    scale_y_continuous(expand=c(0, 0)) +
                                                                                    scale_fill_brewer(palette='Set1', name='Bucket type') +
                                                                                    labs(x='', y=paste(input$currency, '/ Gb / month'), title='Price per "Action"') +
                                                                                    geom_text(aes(y=USD * 1.1, label="")) # adds a blank layer to increase space on top (Y-axis)
                                                    
                                                } else if (input$view == 'Bucket') { ggplot(df, aes(x=Action, y=USD, fill=Action)) +
                                                                                            geom_bar(stat='identity', position=position_dodge(), lwd=.3, col='black', alpha=.6) +
                                                                                            cowplot::theme_cowplot() + theme(axis.text.x=element_text(angle=45, hjust=1)) +
                                                                                            facet_grid(~Bucket) +
                                                                                            scale_y_continuous(expand=c(0, 0)) +
                                                                                            scale_fill_brewer(palette='Paired', name='Action') +
                                                                                            labs(x='', y=paste(input$currency, '/ Gb / month'), title='Price per storage type (aka bucket)') +
                                                                                            geom_text(aes(y=USD * 1.1, label=""))
                                                    
                                                } else { heatmap_df <- bucket_nominal_df() %>%
                                                                       column_to_rownames('Bucket') %>%
                                                                       { if (input$currency == 'DKK') .*6.34 else . }
                                                         
                                                         pheatmap::pheatmap(heatmap_df, cluster_rows=F, cluster_cols=F, cellheight=50, cellwidth=50, main=paste0('Heatmap: Price per "Action"\n(', input$currency, '/ Gb / month)'),
                                                                            display_numbers=T, number_color='white', fontsize_number=12, color=heatmap_colors(40))
                                                        }
                                                })

    
    # S4. Nominal Pricing TABLE
    output$nominal_pricing_table <- renderTable({
                                                bucket_nominal_df() %>%
                                                column_to_rownames('Bucket') %>%
                                                if (input$currency == 'DKK') multiply_by(., 6.34) else .
                                                }, striped=T, bordered=T, hover=T, spacing='xs', width='100%', digits=4, align='c', rownames=T)
    
    # S5. Calculated Price TABLE
    bucket_table <- reactive({
                              df <- bucket_nominal_df() %>%
                                    mutate('Storage'=Storage * input$storage_vol * input$months,
                                           'Retrieval'=Retrieval * input$retrieval_vol * input$months,
                                           'Class_A'=Class_A * input$classA_ops * input$months,
                                           'Class_B'=Class_B * input$classB_ops * input$months) %>%
                                    mutate('TOTAL'=rowSums(select(., -Bucket)))
                            })
    
    output$table_prices <- renderTable({    if (input$currency == 'DKK') {
                                                bucket_table() %>%
                                                column_to_rownames('Bucket') %>%
                                                multiply_by(6.34) %>%
                                                rename(`TOTAL (DKK)`='TOTAL')
                                                } else {
                                                bucket_table() %>%
                                                column_to_rownames('Bucket') %>%
                                                rename(`TOTAL (USD)`='TOTAL')
                                                }
                                        }, striped=T, bordered=T, hover=T, spacing='xs', width='100%', align='c', rownames=T)
    
    # S6. Usage Pricing PLOT
    output$priced_plot <- renderPlot({
                                      df <- bucket_table() %>%
                                            select(-matches('TOTAL')) %>%
                                            column_to_rownames('Bucket') %>%
                                            { if (input$currency == 'DKK') multiply_by(., 6.34) else . } %>%
                                            rownames_to_column('Bucket') %>%
                                            mutate('Bucket'=factor(Bucket, levels=bucket_levels)) %>%
                                            melt(id.vars='Bucket', value.name='USD', variable.name='Action')
                                        
                                      title <- paste0('Projected Price (', input$currency, '): ', input$storage_vol, ' Gb for ', input$months, ' months')
                                        
                                      if (input$view == 'Action') { ggplot(df, aes(x=Action, y=USD, fill=Bucket)) +
                                                                           geom_bar(stat='identity', position=position_dodge(), col='black', lwd=.3, alpha=.6) +
                                                                           stat_summary(aes(label=round(stat(y))), fun='sum', geom='text', vjust=-.5, position=position_dodge(width=.9)) +
                                                                           cowplot::theme_cowplot() +
                                                                           labs(x='', y=paste0('Total cost (', input$currency, ')'), title=title) +
                                                                           scale_y_continuous(expand=c(0, 0)) +
                                                                           scale_fill_brewer(palette='Set1', name='') +
                                                                           geom_text(aes(y=USD * 1.1, label=""))
                                          
                                      } else if (input$view == 'Bucket') { ggplot(df, aes(x=Action, y=USD, fill=Action)) +
                                                                                  geom_bar(stat='identity', position=position_dodge(), lwd=.3, col='black', alpha=.6) +
                                                                                  stat_summary(aes(label=round(stat(y))), fun='sum', geom='text', vjust=-.5) +
                                                                                  cowplot::theme_cowplot() +
                                                                                  theme(axis.text.x=element_text(angle=45, hjust=1)) +
                                                                                  facet_grid(~Bucket) +
                                                                                  scale_y_continuous(expand=c(0, 0)) +
                                                                                  labs(x='', title=title,
                                                                                       y=paste0('Total cost (', input$currency, ')')) +
                                                                                  
                                                                                  scale_fill_brewer(palette='Paired', name='Action') +
                                                                                  geom_text(aes(y=USD * 1.1, label=""))
                                          
                                      } else { heatmap_df <- bucket_table() %>%
                                                             select(-matches('TOTAL')) %>%
                                                             column_to_rownames('Bucket') %>%
                                                             { if (input$currency == 'DKK') multiply_by(., 6.34) else . }
                                              
                                               heatmap_sum_anot <- data.frame(row.names=rownames(heatmap_df), 
                                                                             'TOTAL'=rowSums(heatmap_df))
                                              
                                               pheatmap::pheatmap(heatmap_df, cluster_rows=F, cluster_cols=F, cellheight=50, cellwidth=50, main=paste0('TOTAL (', input$currency, ')'),
                                                                  display_numbers=T, number_format="%.f", number_color='white', fontsize_number=12, color=heatmap_colors(40), annotation_row=heatmap_sum_anot)
                                             }
                                    })
    
}


########## RUN ##########
shinyApp(ui=ui, server=server)
