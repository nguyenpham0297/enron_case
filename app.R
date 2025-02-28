# title: "Enron Email Analysis"
# course: "R for Big Data"
# author: "Phuc Nguyen PHAM"
# instructor: "Pr. Charles Bouveyron"
# date: "2024-11-24"


# Load data
load("Enron.Rdata")

head(employeelist)
head(message)
head(recipientinfo)
str(referenceinfo)

#install the package
library(dplyr)
library(tidyr)
library(lubridate)

#Create Emails Table

#First of all, we will do the data transformation for the "employeelist" table. 

#We change the type of the “status” column to character instead of factor.
employeelist$status = as.character(employeelist$status)

#We also change the “eid” column from "num" to "int" because IDs are whole numbers and do not have decimal numbers.
employeelist$eid = as.integer(employeelist$eid)

#We will create a new table, namely "Emails":
emails = employeelist %>% pivot_longer( #Unpivot all the emails columns
  cols = c(Email_id, Email2, Email3, EMail4),
  names_to = "email",   
  values_to = "email_id") %>% #We rename the new column to "email_id"
  
  #We don't select the "folder" column as it is useless
  select(email_id, eid, firstName, lastName, status) %>% 
  
  #Remove rows with empty emails, because there are some cases where some people only have one email, so the Email2, Email3, Email4 columns will be empty.
  filter(email_id != "") %>% 
  
  #We rename the "eid" column to "employee_id" so it will be easier to read
  rename(employee_id = eid) %>% 
  
  #For the "status" column, we will re-label it, if the employees don't have the title (status is NA or empty), they will be labeled as "Unknown", if they have it, we will keep the status's value.
  mutate(status = trimws(status)) %>% 
  mutate(status = ifelse(
    is.na(status) | 
      toupper(status) == "N/A" | 
      toupper(status) == "NA" | 
      status == "" | 
      is.null(status), "Unknown", status ))

#We will remove the "employeelist" to save the stockage
rm(employeelist)

print(emails)


#Check if there are any duplication for the "email_id" column 
any(duplicated(emails$email_id))

# We create a query to make a list of emails that are possessed by more than one employee.
duplicated_emails = emails %>% 
  group_by(email_id) %>% 
  filter(n_distinct(employee_id) > 1)

# Check whether the emails are used exclusively by one employee
nrow(duplicated_emails) ==0

#Remove the table that we have created from the environment to save space
rm(duplicated_emails)


#Create the "activities" table

#Check if there is any duplication for the "mid" column in the reference table
any(duplicated(referenceinfo$mid))

#Now we will join the tables

#We use the left join function to 
merged_message = left_join(message, referenceinfo, by = "mid")

merged_recipient = left_join(recipientinfo, merged_message, by = "mid")

activities = merged_recipient %>%
  mutate(activity_id = row_number())

#We wil create the activities table
activities = activities %>%
  
  #First of all, we change the types and names of the columns (email of recipient, email of sender ans id of message, type of recipient) so we can read easily
  mutate(rvalue = as.character(rvalue)) %>%
  rename(email_recipient = rvalue) %>%
  mutate(sender = as.character(sender)) %>%
  rename(email_sender = sender) %>%
  mutate(rtype = as.character(rtype)) %>%
  rename(recipient_type = rtype) %>%
  mutate(mid = as.integer(mid)) %>%
  rename(mes_id = mid) %>%
  
  #Then, we remove some columns that are unnecessary.
  select(-rfid) %>%
  select(-rid) %>%
  select(-message_id) %>%
  
  #We also transform the date type date, and create 3 columns to express the year, month and day of the message
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(year = year(date),month = month(date),day = day(date)) %>%
  mutate(year = as.integer(year), month = as.integer(month), day = as.integer(day)) %>%
  
  #For the "subject" column, we know that we can send, respond, or forward a message, so we will create 2 columns : 
  
  #A subject type column which helps to categorizes the subject, if it begins with "RE:", so it means people responded an email, if it begins with "FW:", it means people forwarded an email, if not, it means people sent an email. We also use "grepl" for matching and replacement, and ignore.case to ignore lower case and upper case differences. 
  mutate(subject_type = ifelse(grepl("^RE:", subject,ignore.case = TRUE), "RE",
                               ifelse(grepl("^FW:", subject, ignore.case = TRUE), "FW","Send"))) %>%
  
  #We create the subject content column in order to show only the content of the subject, regardless it is a normal, responded or forwarded email, and we will trim and make it to be a lowercase.
  mutate(subject_content = ifelse(subject_type == "RE", sub("^RE:", "", subject, ignore.case = TRUE),
                                  ifelse(subject_type == "FW", sub("^FW:", "", subject, ignore.case = TRUE), subject))
  ) %>%
  mutate(subject_content = trimws(subject_content)) %>%
  mutate(subject_content = tolower(subject_content)) %>%
  select(activity_id, everything())

#Remove the table to save space
rm(message, merged_message,recipientinfo,merged_recipient, referenceinfo)

#Show the result
head(activities)


is_cleaned = function(x) { !(
  anyNA(x) | 
    any(toupper(x) == "N/A") | 
    any(toupper(x) == "NA") | 
    any(x == "") | 
    any(is.null(x))
)}


is_cleaned(activities$mes_id)
is_cleaned(emails$email_id)
is_cleaned(emails$employee_id)
is_cleaned(activities$email_sender)

summary(activities)
summary(emails)

library(ggplot2)
library(gridExtra)


#Create the table to calculate the percentage of each status
status_percentage = emails %>%
  distinct(employee_id, .keep_all = TRUE) %>%
  group_by(status) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100)) %>%
  arrange(desc(percentage))

# Create the pie chart of status

#This library will help to sort the portion
library("forcats") 
#We sort the "status" by the value of "percentage"
Status = fct_infreq(status_percentage$status,w= status_percentage$percentage)

ggplot(status_percentage, aes(x="", y=percentage, fill= Status))+
  geom_bar(width = 1, stat = "identity", color = "white")+ 
  coord_polar("y", start=0, direction = -1)+
  theme_minimal()+
  ggtitle("Status Distribution of 149 Employees") +
  geom_text(aes(x=1.3, label=paste(percentage,"%")),position = position_stack(vjust=0.5))


most_popular_message_id = activities %>%
  group_by(mes_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=10) 

most_popular_message =  left_join(most_popular_message_id, activities, by = "mes_id") %>% 
  select(mes_id, count, subject) %>%  
  distinct(mes_id, .keep_all = TRUE) %>% 
  arrange(desc(count)) 

most_popular_message_plot = ggplot(most_popular_message, aes(x = reorder(interaction(mes_id, subject),count) , y = count)) +
  geom_bar(stat = "identity", fill = "#87CEEB") +
  labs(title = "Top 10 popular messages",
       x = "Messageid - Name of the message",
       y = "Number of message") +
  coord_flip() +
  geom_text(aes(label = count), vjust = 0)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


most_popular_subject = activities %>%
  filter(subject_content != "") %>%
  group_by(subject_content) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=15) 


most_popular_subject_plot = ggplot(most_popular_subject, aes(x = reorder(subject_content,count) , y = count)) +
  geom_bar(stat = "identity", fill = "#87CEEB") +
  labs(title = "Most popular subject",
       x = "Subject",
       y = "Number of message") +
  coord_flip() +
  geom_text(aes(label = count), vjust = 0)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

most_send_emails = activities %>%
  group_by(email_sender) %>%
  summarise(count =n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

#Create the histogram
most_send_emails_plot =  
  ggplot(most_send_emails, aes(x = reorder(email_sender, count), y = count)) +
  geom_bar(stat = "identity", fill = "#87CEEB") +
  labs(title = "Top 10 email senders",
       x = "Sender",
       y = "Number of email") +
  coord_flip() +
  geom_text(aes(label = count), hjust = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


most_send_employee = activities %>%
  left_join(emails,by = c("email_sender" = "email_id"))%>%
  mutate(full_name = paste(firstName,lastName)) %>%
  group_by(full_name) %>%
  summarise(count =n()) %>%
  arrange(desc(count)) %>%
  slice_head(n=15) 

most_send_employee_plot = ggplot(most_send_employee, aes(x = reorder(full_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Most active Enron's employees",
       x = "Name of employee",
       y = "Number of email sent") +
  coord_flip() +
  geom_text(aes(label = count), hjust = 0) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#Calculate sent emails per status
sent_emails = activities %>%
  group_by(email_sender) %>%
  summarise(sent_count = n()) %>%
  left_join(emails, by = c("email_sender" = "email_id")) %>%
  
  #After the joining process, there are cases that the status has the NA value because some emails don't exist in the "emails" table, so we have to switch those NA values to "Unknown", and since it creates the noise, so we will drop this value
  replace_na(list(status = "Unknown")) %>% 
  filter(status != "Unknown") %>% 
  
  #There are a lot of extreme value (noise) so we decided to select the number of sent email which is less than 6000 so see easily the chart
  filter(sent_count <= 6000)  

#Calculate received emails per status
received_emails = activities %>%
  group_by(email_recipient) %>%
  summarise(received_count = n()) %>%
  left_join(emails, by = c("email_recipient" = "email_id")) %>%
  replace_na(list(status = "Unknown")) %>% 
  filter(status != "Unknown") %>% 
  
  #There are also a lot of extreme value so the number of sent email which is less than 8000 is selected to see easily the chart
  filter(received_count <= 8000)

#Create box plot for sent emails
sent_plot = ggplot(sent_emails, aes(x = status, y = sent_count)) +
  geom_boxplot(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of emails Sent by status", x = "Status", y = "Number of emails sent")

#Create box plot for received emails
received_plot = ggplot(received_emails, aes(x = status, y = received_count)) +
  geom_boxplot(fill = "lightgreen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of emails received by status", x = "Status", y = "Number of emails received")



library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dslabs)
library(tidyverse)
library(plotly)


# Define UI for application that draws a histogram
ui = dashboardPage(
  dashboardHeader(title = "Enron emails"),
  dashboardSidebar(
    
    #We create the side bar Menu, the tab Name will help to connect with the tab Item, which helps to access to the right chart each time we click on a menu item
    sidebarMenu(
      menuItem("Most active emails", tabName = "most_send"),
      menuItem("Most active employees", tabName = "most_active"),
      menuItem("Most active emails by status", tabName = "emails_status"),
      menuItem("Frequency of messages overtime", tabName = "frequency"),
      menuItem("Number of emails sent by status", tabName = "sent_status_overtime"),
      menuItem("Most discussed subjects", tabName = "subject_overtime"))),
  dashboardBody(
    tabItems(
      
      #We create the UI for the question of most active mails
      tabItem(tabName = "most_send", fluidRow(
        box(plotOutput("most_send"), width = 12))),
      
      #Then, we will create the UI for the question of most active mails by status and add 2 action buttons
      tabItem(tabName = "emails_status", fluidRow(
        actionButton("status_sent", "Show sent emails plot"),
        actionButton("status_received", "Show received emails plot"),
        box(plotOutput("emails_status"), width = 12))),
      
      #We create the UI for the question of most active employees
      tabItem(tabName = "most_active", fluidRow(
        box(plotOutput("most_active_employees"), width = 12))),
      
      #Here is the UI for the question of temporal dynamic of the messages
      tabItem(tabName = "frequency",fluidRow(
        box(sliderInput("year_range", "Choose the periode:", min = 1999, max = 2002, value = c(2000, 2001)), width = 9, height = "100px"),
        box(plotOutput("messages_overtime"), width = 12))),
      
      #Create the UI for the sent emails by status overtime
      tabItem(tabName="sent_status_overtime",fluidRow(
        box(sliderInput("year_range_1", "Select the year range:", min = 1999, max = 2002, value = c(2000,2001)), height = "100px"), 
        box(selectInput("status_choice","Select the status (multiple choice):", choices = unique(emails %>% distinct(status) %>% filter(status != "Unknown")), multiple = TRUE, selected = "CEO"), height = "100px"),
        box(plotOutput("sent_status_overtime"), width = 12))),
      
      #Create the UI for the most popular subject over time
      tabItem(tabName = "subject_overtime",fluidRow(
        
        #We add a slicer to choose the month range
        box(sliderInput("month_range", "Select the month range:", min = 1, max = 12, value = c(9,10)), height = "100px", width = 4),
        #Add a choice box to choose the year
        box(selectInput("year_choice","Select the year:", choices = c(1999,2000,2001,2002), multiple = FALSE, selected = 2001), height = "100px", width = 4),
        #Add a text input to filter the subject containing the words that we input
        box(textInput("keyword", "Select the keywords:","online"), height = "100px", width = 4),
        box(plotOutput("subject_overtime"), width = 12)
      )
      
      ))))




# Define server logic required to draw a histogram
server = function(input, output) {
  
  output$most_send = renderPlot({
    most_send_emails_plot
  })
  
  output$emails_status = renderPlot({
    sent_plot
  })
  
  observeEvent(input$status_sent, {
    output$emails_status = renderPlot({ sent_plot })
  })
  
  observeEvent(input$status_received, {
    output$emails_status = renderPlot({ received_plot })
  })
  
  output$most_active_employees = renderPlot({
    most_send_employee_plot
  })
  
  output$messages_overtime = renderPlot({
    
    #We create the calculation and the plot for the question of temporal dynamic of the messages
    messages_overtime_count = activities %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      group_by(date) %>%
      summarise(count = n()) %>%
      mutate(year = year(date))
    
    #Make the plot for the calculation above
    messages_overtime_plot = ggplot(data = messages_overtime_count, aes(x = date, y = count)) +
      geom_line(color = "#3E4E68") +
      labs(title = "Temporal dynamic of the messages",
           y = "Number of messages",
           x = "Date") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    messages_overtime_plot})
  
  #We create the calculation and plot for the sent mails by status overtime
  output$sent_status_overtime =renderPlot({
    
    #Create the calculation of sent emails over time
    sent_overtime = activities %>%
      filter(year >= 1999 & year <= 2002) %>%
      left_join(emails, by = c("email_sender" = "email_id")) %>%
      filter(status != "Unknown") %>%
      group_by(date,status) %>%
      summarise(count = n()) %>%
      ungroup()
    
    #We have to integrate the year range in the calculation 
    filtered_sent_status = sent_overtime %>%
      filter(year(date) >= input$year_range_1[1] & year(date) <= input$year_range_1[2]) %>%
      filter(status %in% input$status_choice)
    
    #Make the plot of sent emails over time
    sent_overtime_plot = ggplot(data = filtered_sent_status, aes(x=date, y=count, color = status)) +
      geom_line() +
      labs(title = "Temporal dynamic of the sent emails",
           y = "Number of message",
           x = "Date") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    sent_overtime_plot
  })
  
  #Create the calculation and plot for most popular subjects overtime
  output$subject_overtime = renderPlot({
    
    most_popular_subject = activities %>%
      filter(year %in% input$year_choice) %>%
      filter(subject_content != "") %>%
      group_by(date,subject_content) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice_head(n=15) 
    
    most_popular_subject_1 = most_popular_subject %>%
      filter(month(date) >= input$month_range[1] & month(date) <= input$month_range[2]) 
    
    #We will make a condition, if the keyword is empty, we won't filter data, if we input some words in the box, it will match and filter the subject containing those words
    if (input$keyword != "") 
    {most_popular_subject_1 = most_popular_subject_1 %>%
      filter(grepl(input$keyword,subject_content))}
    
    most_popular_subject_2 = most_popular_subject_1 %>%
      group_by(subject_content) %>%
      summarise(count = sum(count)) %>%
      arrange(desc(count)) %>%
      slice_head(n = 15)
    
    #Create the plot of most popular subjects
    most_popular_subject_plot = ggplot(most_popular_subject_2, aes(x = reorder(subject_content,count) , y = count)) +
      geom_bar(stat = "identity", fill = "#87CEEB") +
      labs(title = "Most popular subject",
           x = "Subject",
           y = "Number of message") +
      coord_flip() +
      geom_text(aes(label = count), vjust = 0)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    most_popular_subject_plot})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
