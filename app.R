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
    h4('Based on Regional location (Finland or Netherlands)'),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=2,
                     radioButtons(inputId='view', label='View', choices=list('Action', 'Bucket', 'Heatmap'), selected='Action'),
                     radioButtons(inputId='currency', label='Currency', choices=list('USD', 'DKK'), selected='USD', inline=T),
                     sliderInput(inputId='months', label='Months:', min=1, max=36, value=1, step=1),
                     sliderInput(inputId='storage_vol', label='Storage volume (GBs):', min=0, max=5000, value=1, step=250),
                     sliderInput(inputId='retrieval_vol', label='Retrieval volume (GBs):', min=0, max=5000, value=1, step=50),
                     sliderInput(inputId='classA_ops', label='Class A operations (x1000):', min=1, max=25, value=1, step=1),
                     sliderInput(inputId='classB_ops', label='Class B operations (x1000):', min=1, max=25, value=1, step=1),
                     br(), 'Axel Thieffry', br(), 'August 2020 (v1.0)'),
    # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                    tabPanel(title='GCP Buckets',
                             h1('This is H1'),
                             h2('This is H2'),
                             h3('This is H3'),
                             'TODO: fill in this page with types of Bucket, their pros/cons, redundancy, speed, and detail the types of operations.',
                             'Also add some links towards the official GCP documentation and pricing list.'),
                    tabPanel(title='Nominal Pricing',
                             plotOutput('nominal_pricing_plot'),
                             textOutput('txtout_1'),
                             textOutput('txtout_2'),
                             textOutput('txtout_3'),
                             textOutput('txtout_4'),
                             h4(textOutput('title_1')),
                             tableOutput('nominal_pricing_table')),
                    tabPanel(title='Cost Projection',
                             plotOutput('priced_plot'),
                             tableOutput('table_prices'))
                        )
                  )
                )
    )


########### SERVER ###########
##############################
server <- function(input, output) {
    
    # make heatmap color palette
    heatmap_colors <- colorRampPalette(colors=c('white', 'navy', 'red'))
    
    # S0. Layout text
    output$txtout_1 <- renderText({'Storage & Retrieval: USD / Gb / month'})
    output$txtout_2 <- renderText({'Class A & B operations: USD / 1000 operations'})
    output$txtout_3 <- renderText({'ex. Class A: upload objects (file), modify permissions'})
    output$txtout_4 <- renderText({'ex. Class B: view metadata, retrieve bucket & permissions'})

    # S1. make data frame with nominal bucket pricing
    # data gathered from Google "Cloud Console > Storage > Create a Bucket"
    # based on Regional type for europe-north1 (Finland)
    bucket_levels <- factor(c('STANDARD', 'NEARLINE', 'COLDLINE', 'ARCHIVE'))
    bucket_nominal_df <- data.frame('Bucket'=bucket_levels,
                                    'Storage'=c(0.02, 0.01, 0.004, 0.0012),
                                    'Retrieval'=c(0, 0.01, 0.02, 0.05),
                                    'Class_A'=c(0.005, 0.01, 0.01, 0.05),    # for 1000 operations
                                    'Class_B'=c(0.0004, 0.001, 0.005, 0.05)) # for 1000 operations

    # S2. Nominal Pricing PLOT
    output$nominal_pricing_plot <- renderPlot({
                                                df <- bucket_nominal_df %>%
                                                    melt(id.vars='Bucket', variable.name='Action', value.name='USD') %>%
                                                    mutate('Bucket'=factor(Bucket, levels=bucket_levels)) %>%
                                                    { if (input$currency == 'DKK') mutate(., 'USD'=USD*6.34) else . }
                                                
                                                if (input$view == 'Action') {ggplot(df, aes(x=Action, y=USD, fill=Bucket)) +
                                                                                    geom_bar(stat='identity', position=position_dodge(), lwd=.3, col='black', alpha=.6) +
                                                                                    cowplot::theme_cowplot() +
                                                                                    scale_y_continuous(expand=c(0, 0)) +
                                                                                    scale_fill_brewer(palette='Dark2', name='Bucket type') +
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
                                                    
                                                } else { heatmap_df <- bucket_nominal_df %>%
                                                                       column_to_rownames('Bucket') %>%
                                                                       { if (input$currency == 'DKK') .*6.34 else . }
                                                         
                                                         pheatmap::pheatmap(heatmap_df, cluster_rows=F, cluster_cols=F, cellheight=50, cellwidth=50, main=paste0('Heatmap: Price per "Action"\n(', input$currency, '/ Gb / month)'),
                                                                            display_numbers=T, number_color='white', fontsize_number=12, color=heatmap_colors(40))
                                                        }
                                                })

    
    # S3. Nominal Pricing TABLE
    output$nominal_pricing_table <- renderTable({
                                                if (input$currency == 'USD') bucket_nominal_df %>% column_to_rownames('Bucket')
                                                else bucket_nominal_df %>%
                                                        column_to_rownames('Bucket') %>%
                                                        multiply_by(6.34)
                                                }, striped=T, bordered=T, hover=T, spacing='xs', width='100%', digits=2, align='c', rownames=T)
    
    # S4. Calculated Price TABLE
    bucket_table <- reactive({
                              df <- bucket_nominal_df %>%
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
                                        }, striped=T, bordered=T, hover=T, spacing='xs', width='100%', digits=2, align='c', rownames=T)
    
    # S5. Usage Pricing PLOT
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
