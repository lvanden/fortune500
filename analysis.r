library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)

  # read in csv file
  company_list <- read_csv("fortune_500_companies.csv")
  
  # parse DMARC record ----
  company_df <- company_list %>%
    filter(rank <= 100) %>%
    dplyr::mutate(
      # remove quotes around record and remove new lines
      record_string = str_replace_all(gsub('"', '', dmarc_record), '\\n', ''),
      policy = str_extract(dmarc_record, "p=[a-z]+"),
      pct = str_extract(dmarc_record, "pct=\\d+"),
      rua = str_extract(record_string, "rua=mailto:[^;]+"),
      ruf = str_extract(record_string, "ruf=mailto:[^;]+")
    )
  
  # rua domains ----
  rua_df <- company_df %>%
    select(company, policy, domain, rank, rua) %>%
    mutate(
      # split data for multiple addresses
      rua_domains = strsplit(rua, split = ","),
      policy = case_when(
        is.na(policy) ~ "no DMARC record",
        TRUE ~ policy
      )
    ) %>%
    arrange(policy, company) 
  
  #give each company an id
  rua_df$id = 1:nrow(rua_df)
  
  # transform data from wide to long
  rua_df = data.frame(
    company = rep(rua_df$company, sapply(rua_df$rua_domains, length)),
    policy = rep(rua_df$policy, sapply(rua_df$rua_domains, length)),
    domain = rep(rua_df$domain, sapply(rua_df$rua_domains, length)),
    rank = rep(rua_df$rank, sapply(rua_df$rua_domains, length)),
    rua = rep(rua_df$rua, sapply(rua_df$rua_domains, length)),
    id = rep(rua_df$id, sapply(rua_df$rua_domains, length)),
    rua_domain = unlist(rua_df$rua_domains)
  )
  # trim rua down to just domain
  rua_df <- rua_df %>%
    group_by(company) %>%
    mutate(
      rua_count = row_number(),
      rua_domain = str_extract(rua_domain, "\\w*.\\w*$"),
      rua_count = as.character((rua_count)),
      value = case_when(
        is.na(rua_domain) ~ 30,
        TRUE ~ 30
      ),
      # add report receivers
      rua_id = dplyr::case_when(
        is.na(rua_domain) ~ NA_real_,
        rua_domain == "dmarcian.com" | rua_domain == "dmarcian.eu" ~ 2,
        rua_domain == "agari.com" ~ 3,
        rua_domain == "proofpoint.com" ~ 4,
        rua_domain == "vali.email" ~ 5,
        rua_domain == "cisco.com" ~ 6,
        rua_domain == "returnpath.net" ~ 7,
        TRUE ~ 1
      ),
      # add policy id
      policy_id = dplyr::case_when(
        policy == "p=none" ~ 1,
        policy == "p=quarantine" ~ 2,
        policy == "p=reject" ~ 3,
        TRUE ~ 0
      )
    )
  # make rua_id discrete for ggplot
  rua_df$rua_id <- as.factor(rua_df$rua_id)
  
  # create look up table for DMARC report receivers
  report_receivers = data.frame(
    name = c('self', 'dmarcian', 'Agari', 'Proofpoint', 'Valimail', 'Cisco', 'Validity'),
    name_id = c(1:7)
  )


  
  # Viz ----
  data <- rua_df

  # Get the name and the y position of each label (company name)
  label_data <- data %>% group_by(id, company) %>% summarize(tot=sum(value))
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(policy) %>% 
    summarize(start=min(id), end=max(id) ) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # Make the plot
  p <- ggplot(data) +      
    
    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill=rua_id), stat="identity") +
    scale_fill_viridis(
      option = "plasma",
      discrete = TRUE,
      name = "rua",
      labels = c("self", 
                 "dmarcian", 
                 "Agari", 
                 "Proofpoint", 
                 "Vailmail", 
                 "Cisco", 
                 "Validity", 
                 "no reporting"
                )
    ) +
    ylim(-200,max(label_data$tot+50, na.rm=T)) +
    theme_minimal() +
    theme(
      legend.key = element_rect(fill = "white", colour = "black"),
      legend.title = element_text(color = "black", size = 18),
      legend.text = element_text(color = "black", size = 14),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() +
    labs(title = "2020 DMARC Status for the Top 50 Fortune '500' Companies") +

    # Add labels on top of each bar (company name)
    geom_text(data=label_data, aes(x=id, y=tot+10, label=company, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=7, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information (policy groups)
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=policy), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=6, fontface="bold", inherit.aes = FALSE)
  
  # Save as png
  ggsave(p, file="output.png", width=25, height=18)
  
  