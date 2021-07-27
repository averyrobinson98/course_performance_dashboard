library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(viridis)
library(htmlwidgets)
library(htmlTable)
library(htmltools)
library(validate)
library(tidyverse)
library(sparkline)
library(formattable)

# data for courses
all <- read.csv("/Users/averyrobinson/R\ Projects/course_performance_dashboard/course_data.csv")
all <- all[,-1]

# data for prerequisites
req_info <- read.csv("/Users/averyrobinson/R\ Projects/course_performance_dashboard/req_info.csv")

# clean data
all$student_level <- as.numeric(all$student_level)
all$admit_level[all$admit_level == ""] <- NA
all$admit_level <- as.factor(all$admit_level)

order <- c("A+","A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F")
grade_range <- data.frame("grade"=order, "range"=c(rep("A",3), rep("B",3),rep("C",3),rep("<D",4)))
grade_range_col <- data.frame("range"=c("A","B","C","<D"),colors = c("#FDB32FFF","#DE5F65FF","#B52F8CFF","#5D01A6FF"))


colors <- data.frame("grade"=order, "color"= rev(plasma(13)) ,"outline"="white")
set.seed(6)
colors_filter = sample(colors$color, 5)

# select columns we want
filters <- names(all)
filters = filters[-c(2:18, 22:23,25,27,29,31,33,36:37)]

course_cat_name <- paste(all$subj_area_cd, all$crs_catlg_no)
all$course_cat_name <- course_cat_name

body <-dashboardBody(
    tabItems(
        tabItem(
            tabName = "overview",
            fluidRow(
                column(width = 4,
                       box(
                           title = "Select Inputs", width = NULL, solidHeader = TRUE,
                           selectInput('course', "Course Code", levels(as.factor(course_cat_name))), 
                           selectInput('filter','Filter',levels(as.factor(filters)))
                       )),
                column(width = 12,offset=1,
                       box(
                           title = "Grade Distribution Overview", solidHeader = TRUE, width=10,
                           plotOutput("grade_dist")
                       )
                )
            ) 
        ),
        tabItem(
            tabName = "conditional",
            fluidRow(
                column(width = 4,
                       box(
                           title = "Select Inputs", width = NULL, solidHeader = TRUE,
                           selectInput("course1_grade", "Course #1 Grade", selected="B",levels(as.factor(all$grade))),
                           selectInput('course1', "Course #1",selected="MATH 0032A", levels(as.factor(course_cat_name))),
                           selectInput('course2', "Course #2", selected="MATH 0032B",levels(as.factor(course_cat_name)))
                       ),
                       box(
                           title = "Prerequisites for Course 2", width=NULL, solidHeader = TRUE,
                           textOutput("Req_info")
                           
                       ),
                       box(
                           title = "Course Description for Course 2", width=NULL,solidHeader = TRUE,
                           textOutput("Course_Info")
                       ),
                       box(
                           title="Grade Range Proportions for Course 2 given Course 1 Grade",
                           width=NULL,solidHeader = TRUE,
                           plotOutput("pie")
                       )),
                column(width=4,
                       box(
                           title = "Course 1 Grade Distribution", width = NULL, solidHeader = TRUE, 
                           plotOutput("compPlot1")
                       ),
                       box(title = "Course 2 Grade Distribution", width = NULL, solidHeader = TRUE,
                           plotOutput("compPlot2")
                       )
                ),
                column(width=4,
                       box(
                           title="Courses Taken Alongside Course 2",width=NULL, solidHeader = TRUE,
                           selectInput('majordiv', "By Major Division", selected="All Major Divisions",levels(as.factor(c("All Major Divisions",all$maj_div_title)))),
                           selectInput('major', "By Major", selected="All Majors",levels(as.factor(c("All Majors",all$major_name)))),
                           checkboxInput("checkbox","By Course 1 Grade",value=FALSE), 
                           htmlOutput("table_classes")
                       ),
                       box(title="Explanatory Ability (R Squared):", width=NULL, solidHeader = TRUE,
                           textOutput("Rsqval"),
                           textOutput("Expl_Rsq")
                           
                       ))
            )
        )
        
    ))

ui <- dashboardPage(
    dashboardHeader(title = "Course Performance Dashboard", titleWidth = 350),
    dashboardSidebar(
        sidebarMenu(
            fluidPage(menuItem('Course Combination Analysis', tabName = 'conditional', icon = icon('dashboard'))),
            menuItem('Course Overview', tabName = 'overview', icon = icon('dashboard'))
        )),
    body,
    tags$head(
        tags$style(HTML('
                    /* logo */
                    .skin-blue .main-header .logo {
                                background-color: #005587;
                    }
                    /* navbar (rest of the header) */
                    .skin-blue .main-header .navbar {
                                background-color: #005587;
                    }
                    /* main sidebar */
                    .skin-blue .main-sidebar {
                                background-color:#005587;
                    }
                    /* active selected tab in the sidebarmenu */
                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #003B5C;
                    }
                    /* body */
                    .content-wrapper, .right-side {
                                background-color:  #DAEBFE;
                    }
    '))
    )
)




# Define server logic required to draw features
server <- function(input, output) {
    
    
    # main histogram in second tab
    output$grade_dist<- renderPlot({ 
        filter_column <- which(colnames(all) == input$filter)
        selected_filter <- colnames(all)[which(colnames(all) == input$filter)]
        dat <- all %>% filter(course_cat_name == input$course) %>% mutate(grade = factor(grade,levels=order))
        
        factors <- dat %>% count(dat[,filter_column], sort = TRUE)
        top_factors <- factors[1:5,1]
        
        dat2 <- dat[(dat[,filter_column] %in% top_factors),]
        dat2[,filter_column] <- as.factor(dat2[,filter_column])
        dat2 <- dat2 %>% filter(!is.na(dat2[,filter_column]))
        
        labels <- names(table(dat2[,filter_column]))
        label_props <- round(100 * table(dat2[,filter_column]) / sum(table(dat2[,filter_column])),2)
        num_facs = length(labels)
        num_facs = length(num_facs)
        
        order= names(sort(label_props,decreasing = T))
        label_props <- sort(label_props,decreasing = T)
        labels <- names(label_props)
        
        ggplot(dat2, aes(x = grade, fill = factor(dat2[,filter_column], levels=order))) +
            geom_bar(position = 'dodge',aes(y = (..count..)/sum(..count..))) + 
            theme_minimal()+
            scale_y_continuous(labels = scales::percent)+
            labs(title=paste0("Grade Distribution for: ", unique(all[course_cat_name == input$course,"course_title"])) ,
                 subtitle = paste0(unique(all[course_cat_name == input$course,"course_level"])," ",unique(all[course_cat_name == input$course,"course_dept_title"]),
                                   ", ", unique(all[course_cat_name == input$course,"course_div_title"]) ),
                 caption="Grouped by Maximum Top 5 Levels of Filter") +
            theme(plot.title = element_text(size=16, hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5), 
                  axis.title.x = element_blank(), axis.text.x = element_text(size = 10), 
                  axis.title.y = element_blank(), axis.text.y = element_text(size = 10),
                  legend.title = element_text(size = 12), legend.text = element_text(size = 12),
                  plot.caption = element_text(size = 14)) +
            scale_fill_viridis(discrete=TRUE,name=input$filter,
                               labels= paste(labels,":  ",
                                             paste0(label_props,"%")) , option="plasma",end=.9,direction = -1)
        
        
    })
    
    # incorporate warning 
    output$warning <- renderText(
        if(any(filters_w_warnings == input$filter)){
            print("The number of levels for this filter is too large. Only the five most common levels for this filter are shown in the plot.")
        }
    )
    
    
    output$table_classes <- renderUI({
        
        if(input$major == "All Majors" & !input$checkbox){
            t= all %>% filter(course_cat_name == input$course2) %>%
                filter(
                    if (input$majordiv != "All Major Divisions") {
                        maj_div_title == input$majordiv
                    } else {
                        maj_div_title == maj_div_title
                    })  %>% 
                select(student_id,term_code) %>% 
                left_join(all, by=c("student_id","term_code")) %>% filter(course_cat_name != input$course2) %>%
                group_by(course_cat_name) %>% summarise("Count" = n()) %>% arrange(desc(Count)) %>% head(10)
            names(t) = c("Course","Count")
        } else if(!input$checkbox){
            t = all %>% filter(course_cat_name == input$course2, major_name == input$major) %>%
                filter(
                    if (input$majordiv != "All Major Divisions") {
                        maj_div_title== input$majordiv
                    } else {
                        maj_div_title == maj_div_title
                    })  %>% 
                select(student_id,term_code) %>% 
                left_join(all, by=c("student_id","term_code")) %>% filter(course_cat_name != input$course2) %>%
                group_by(course_cat_name) %>% summarise("Count" = n()) %>% arrange(desc(Count)) %>% head(10)
            names(t) = c("Course","Count")
        } else if(input$major == "All Majors" & input$checkbox){
            t = all %>% filter(course_cat_name == input$course2, grade == input$course1_grade)%>%
                filter(
                    if (input$majordiv != "All Major Divisions") {
                        maj_div_title== input$majordiv
                    } else {
                        maj_div_title == maj_div_title
                    })  %>% 
                select(student_id,term_code) %>% 
                left_join(all, by=c("student_id","term_code")) %>% filter(course_cat_name != input$course2) %>%
                group_by(course_cat_name) %>% summarise("Count" = n()) %>% arrange(desc(Count)) %>% head(10)
            names(t) = c("Course","Count")
        } else{
            t = all %>% filter(course_cat_name == input$course2, grade == input$course1_grade,
                               major_name == input$major) %>%
                filter(
                    if (input$majordiv != "All Major Divisions") {
                        maj_div_title== input$majordiv
                    } else {
                        maj_div_title == maj_div_title
                    })  %>% 
                select(student_id,term_code) %>% 
                left_join(all, by=c("student_id","term_code")) %>% filter(course_cat_name != input$course2) %>%
                group_by(course_cat_name) %>% summarise("Count" = n()) %>% arrange(desc(Count)) %>% head(10)
            names(t) = c("Course","Count")
        }
        
        
        
        
        
        
        
        vars <- t$Course
        id_and_term = all %>% filter(course_cat_name == input$course2) %>% select(student_id,term_code)
        temp <- left_join(id_and_term,all, by=c("student_id","term_code")) %>% filter(course_cat_name != input$course2)
        grades <- temp %>% filter(course_cat_name %in% vars) %>% select(course_cat_name,grade)
        
        
        
        
        if(length(vars) > 0){
            col = "orange"
            l = vector(mode = "list", length = length(vars))
            
            for(i in 1:length(vars)){
                temporary = table(grades %>% filter(course_cat_name == vars[i])) %>% as.data.frame() %>% select(grade,Freq)
                l[[i]] = as.data.frame(temporary[match(order,temporary$grade),])
            }
            
            sparklines <- rep(NA,nrow(t))
            for(i in 1:nrow(t)){
                sparklines[i] = as.character(htmltools::as.tags(sparkline(l[[i]]$Freq, type = "bar",barColor=col)))
            }
            
            t=data.frame("Course"=pull(t,Course), "Count"=pull(t,Count), "Sparkline"=sparklines)
            # t$Course = spk_chr(t$Course)
            #t$Count = spk_chr(t$Count)
            
            t = t %>%
                format_table(align ="c") %>% htmltools::HTML() %>%
                div() %>% spk_add_deps() %>% fluidRow()
            
            # out = as.htmlwidget(formattable(t))
            #out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
            #out
        } else{
            out <- "NA"
            out
        }
        
        
    })
    
    
    
    
    output$compPlot1 <- renderPlot({
        colorst = colors
        
        unq_grades = all %>% mutate(grade=factor(grade, levels=order)) %>% 
            filter(course_cat_name == input$course1 ) %>% select(grade) %>% unique
        colorst = colorst[colorst$grade %in% unq_grades$grade,]
        
        colorst$color[colorst$grade == input$course1_grade] <- "black"
        
        
        all %>% mutate(grade=factor(grade, levels=order)) %>% filter(course_cat_name == input$course1 ) %>%
            ggplot(aes(x=grade,fill=grade)) +
            geom_bar(aes(y = (..count..)/sum(..count..)),fill=colorst$color,color=colorst$outline,size=1)+
            scale_y_continuous(labels = scales::percent)+
            labs(title =paste(input$course1,"Grade Distribution") ,
                 caption= "Black bar = Course 1 Selected Grade" )+
            theme_minimal()+
            theme(axis.title = element_blank())
        
    })
    
    output$compPlot2 <- renderPlot({
        class_1_uid <- all %>% filter(course_cat_name == input$course1) %>% filter(grade == input$course1_grade) %>% select(student_id)
        # class_1_grade <- dat$course1_grade
        dat <- all %>% 
            filter(course_cat_name == input$course2) %>% 
            filter(student_id %in% class_1_uid$student_id) 
        
        colorst = colors
        
        unq_grades = dat %>% mutate(grade=factor(grade, levels=order)) %>% 
            filter(course_cat_name == input$course2 ) %>% select(grade) %>% unique
        
        colorst = colorst[colorst$grade %in% unq_grades$grade,]
        
        
        validate(
            need(nrow(dat) > 10, "Insufficient data to create conditional plot. Please select another course combination.")
        )
        
        dat %>%  mutate(grade=factor(grade, levels=order)) %>%
            ggplot(aes(x = grade,fill=grade)) +
            geom_bar(aes(y = (..count..)/sum(..count..)), fill=colorst$color) +
            scale_y_continuous(labels = scales::percent)+
            labs(title =paste(input$course2,"Grade Distribution for those who received a(n)",input$course1_grade,"in",input$course1)) +
            theme_minimal()+
            theme(axis.title = element_blank())
        
    })
    
    output$pie <- renderPlot({
        class_1_uid <- all %>% filter(course_cat_name == input$course1) %>% filter(grade == input$course1_grade) %>% select(student_id)
        
        dat <- all %>% 
            filter(course_cat_name == input$course2) %>% 
            filter(student_id %in% class_1_uid$student_id) 
        
        validate(
            need(nrow(dat) > 10, "Insufficient data to create conditional plot. Please select another course combination.")
        )
        
        dat <- inner_join(dat,grade_range,by="grade")
        tab <- prop.table(table(dat$range))
        d_f <- data.frame("GradeRange"=names(tab), "prop"=unname(tab))
        
        
        grade_range_col_st = grade_range_col$colors[grade_range_col$range %in% d_f$GradeRange]
        
        d <- d_f %>% mutate(GradeRange =factor(GradeRange, levels = c("A","B","C","<D")))  
        
        
        ggplot(d,aes(x="", y=prop.Freq,fill=GradeRange))+
            geom_bar(stat="identity", width=1, color="white") +
            scale_fill_manual(values=grade_range_col_st)+
            coord_polar("y", start=0) +
            theme_void()+
            theme (legend.position="top")
        
        
    })
    
    output$Rsqval <- renderText({
        class_1_uid <- all %>% filter(course_cat_name == input$course1)  %>% select(student_id)
        dat <- all %>% 
            filter(course_cat_name == input$course2) %>% 
            filter(student_id %in% class_1_uid$student_id) 
        
        student_ids_both = dat$student_id
        
        validate(
            need(length(student_ids_both) > 10, "Insufficient data for analysis. Please select another course combination.")
        )
        first <- all %>% 
            filter(course_cat_name == input$course1) %>% filter(student_id %in% student_ids_both) %>% 
            select(grade,student_id)
        
        second <- all %>% 
            filter(course_cat_name == input$course2) %>% filter(student_id %in% student_ids_both) %>%
            select(grade,student_id)
        
        both_courses <- inner_join(first,second,by="student_id")
        both_courses <- both_courses %>% select(-student_id)
        both_courses$grade.x <- factor(both_courses$grade.x)
        both_courses$grade.y <- factor(both_courses$grade.y)
        
        myvals <- c(13,12,11,10,9,8,7,6,5,4,3,2,1)
        training_x <- pull(both_courses, grade.x)
        training_x_num <- myvals[training_x]
        training_y <- pull(both_courses, grade.y)
        training_y_num <- myvals[training_y]
        
        
        mod <- lm(training_y_num~training_x_num)
        summ_mod <- summary(mod)
        print(paste(as.character(round(summ_mod$r.squared*100,2)), "%"))
    })
    
    output$Expl_Rsq <- renderText({
        
        print("This value can be interpreted as the amount that a student's course 1 grade can help us predict their course 2 grade.")
        
    })
    
    output$Req_info <- renderText({
        match_to = str_remove_all(input$course2,"[:blank:]")
        char0 = character(0)
        if(identical(req_info$course_cat_name2[req_info$course_cat_name2 == match_to],char0)){
            for(i in 1:3){
                match_to = str_sub(match_to,end=-(i+1))
                if(identical(req_info$course_cat_name2[req_info$course_cat_name2 == match_to],char0)){
                    break
                }
            }
        }
        
        validate(
            need(is.na(req_info[req_info$course_cat_name2==match_to,]$Requisites) == F, "No Prerequisite information available.")
        )
        print(paste(req_info[req_info$course_cat_name2==match_to,]$Requisites))
        
    })
    
    output$Course_Info <- renderText({
        match_to = str_remove_all(input$course2,"[:blank:]")
        char0 = character(0)
        if(identical(req_info$course_cat_name2[req_info$course_cat_name2 == match_to],char0)){
            for(i in 1:3){
                match_to = str_sub(match_to,end=-(i+1))
                if(identical(req_info$course_cat_name2[req_info$course_cat_name2 == match_to],char0)){
                    break
                }
            }
        }
        
        validate(
            need(is.na(req_info[req_info$course_cat_name2==match_to,]$Description) == F, "No Course information available.")
        )
        print(paste(req_info[req_info$course_cat_name2==match_to,]$Description))
        
    })
    
    
    
    
    
    output$table<- DT::renderDataTable({
        dat <- all %>% filter(course_cat_name == input$course1b) 
        class_1_uid <- dat$student_id
        dat <- all %>% 
            filter(course_cat_name == input$course2b) %>% 
            filter(student_id %in% class_1_uid) %>% 
            select(course_cat_name, maj_dept_title, admit_level, student_level, grade)
        
        DT::datatable(dat, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    
}




# Run the application 
shinyApp(ui = ui, server = server)