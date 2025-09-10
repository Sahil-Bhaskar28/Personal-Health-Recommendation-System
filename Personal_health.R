# Install required packages
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("ggplot2")) install.packages("ggplot2")
install.packages('rsconnect')

library(shiny)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Personal Healthcare Recommendation System"),
  tags$style(HTML("
    .tab-content {
      padding: 15px;
    }
    .well {
      background-color: #f9f9f9;
    }
    .shiny-output-error {
      color: red;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Personal Information"),
      textInput("name", "Full Name", placeholder = "John Doe"),
      numericInput("age", "Age", value = 30, min = 1, max = 120),
      selectInput("gender", "Gender", choices = c("Male", "Female", "Other")),
      numericInput("height", "Height (cm)", value = 170, min = 50, max = 250),
      numericInput("weight", "Weight (kg)", value = 70, min = 10, max = 300),
      
      h4("Lifestyle Information"),
      selectInput("activity", "Activity Level", 
                  choices = c("Sedentary", "Lightly Active", "Moderately Active", 
                              "Very Active", "Extremely Active")),
      selectInput("health_goal", "Primary Health Goal",
                  choices = c("Weight Loss", "Weight Gain", "Maintain Weight", 
                              "Muscle Building", "General Health")),
      
      h4("Health Conditions"),
      checkboxGroupInput("conditions", "Existing Health Conditions",
                         choices = c("Diabetes", "Hypertension", "Heart Disease", 
                                     "Arthritis", "Asthma", "Thyroid"),
                         selected = character(0)),
      textAreaInput("symptoms", "Current Symptoms (describe briefly)", 
                    placeholder = "e.g., frequent headaches, fatigue"),
      
      actionButton("submit", "Get Recommendations", class = "btn-primary"),
      hidden(div(id = "loading", 
                 style = "margin-top: 15px;",
                 tags$div(class = "progress",
                          tags$div(class = "progress-bar progress-bar-striped active", 
                                   role = "progressbar",
                                   style = "width: 100%"))))
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Overview",
                 h3("Your Health Summary"),
                 uiOutput("healthSummary"),
                 plotOutput("healthOverviewPlot", height = "400px"),
                 plotOutput("bmiPlot", height = "200px")
        ),
        tabPanel("Diet Plan", 
                 h3("Personalized Diet Chart"),
                 tableOutput("dietPlan"),
                 plotOutput("nutrientPlot")),
        tabPanel("Treatment Advice", 
                 h3("Condition-Specific Recommendations"),
                 uiOutput("treatmentAdvice")),
        tabPanel("Doctor Tips", 
                 h3("Professional Health Tips"),
                 uiOutput("doctorTips")),
        tabPanel("Health Improvement", 
                 h3("Personalized Health Improvement Plan"),
                 uiOutput("healthTips"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  user_data <- reactiveValues(
    bmi = NULL,
    bmi_category = NULL,
    calorie_needs = NULL,
    stored_data = NULL
  )
  
  observeEvent(input$submit, {
    show("loading")
    
    delay(100, {
      tryCatch({
        user_data$bmi <- input$weight / ((input$height/100)^2)
        user_data$bmi_category <- calculate_bmi_category(user_data$bmi)
        user_data$calorie_needs <- calculate_calorie_needs(input)
        
        user_data$stored_data <- list(
          name = input$name,
          age = input$age,
          gender = input$gender,
          height = input$height,
          weight = input$weight,
          bmi = user_data$bmi,
          bmi_category = user_data$bmi_category,
          activity = input$activity,
          health_goal = input$health_goal,
          conditions = input$conditions,
          symptoms = input$symptoms,
          calorie_needs = user_data$calorie_needs
        )
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }, finally = {
        hide("loading")
      })
    })
  })
  
  output$healthSummary <- renderUI({
    req(input$submit, user_data$stored_data)
    
    # Create a list of all elements to display
    elements <- list(
      h4(paste("Hello,", input$name)),
      p(strong("Age:"), input$age, "years"),
      p(strong("Gender:"), input$gender),
      p(strong("BMI:"), round(user_data$bmi, 1), "-", user_data$bmi_category),
      p(strong("Activity Level:"), input$activity),
      p(strong("Health Goal:"), input$health_goal)
    )
    
    # Add conditions if any exist
    if (length(input$conditions) > 0) {
      elements <- c(elements, 
                    list(p(strong("Health Conditions:"), paste(input$conditions, collapse = ", "))))
    }
    
    # Add symptoms if any exist
    if (!is.null(input$symptoms) && nchar(trimws(input$symptoms)) > 0) {
      elements <- c(elements, 
                    list(p(strong("Reported Symptoms:"), input$symptoms)))
    }
    
    # Return the complete tagList
    do.call(tagList, elements)
  })
  
  output$healthOverviewPlot <- renderPlot({
    req(input$submit, user_data$stored_data)
    
    # Prepare health metrics data
    health_metrics <- data.frame(
      Metric = c("BMI", "Daily Calories", "Activity Level", "Health Goal"),
      Value = c(
        round(user_data$bmi, 1),
        round(user_data$calorie_needs),
        factor(input$activity, levels = c("Sedentary", "Lightly Active", 
                                          "Moderately Active", "Very Active", 
                                          "Extremely Active")),
        factor(input$health_goal)
      ),
      Category = c("Physical", "Nutrition", "Lifestyle", "Goal")
    )
    
    # Create bar plot
    ggplot(health_metrics, aes(x = Metric, y = Value, fill = Category)) +
      geom_col() +
      geom_text(aes(label = Value), vjust = -0.5, size = 5) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Your Health Overview",
           x = "",
           y = "",
           fill = "Category") +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "bottom"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  
  output$bmiPlot <- renderPlot({
    req(user_data$bmi)
    
    bmi_categories <- data.frame(
      category = c("Underweight", "Normal", "Overweight", "Obese"),
      min = c(0, 18.5, 25, 30),
      max = c(18.5, 25, 30, 45)
    )
    
    ggplot(bmi_categories, aes(xmin = min, xmax = max, y = 1, fill = category)) +
      geom_rect() +
      geom_vline(xintercept = user_data$bmi, color = "red", size = 1.5) +
      annotate("text", x = user_data$bmi, y = 1.5, 
               label = paste("Your BMI:", round(user_data$bmi, 1)), color = "red") +
      scale_fill_brewer(palette = "Set2") +
      labs(x = "BMI", y = "", fill = "BMI Category") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
  
  output$dietPlan <- renderTable({
    req(input$submit, user_data$stored_data)
    generate_diet_plan(user_data$stored_data)$table
  }, striped = TRUE, hover = TRUE)
  
  output$nutrientPlot <- renderPlot({
    req(input$submit, user_data$stored_data)
    nutrients <- generate_diet_plan(user_data$stored_data)$nutrients
    
    ggplot(nutrients, aes(x = nutrient, y = value, fill = nutrient)) +
      geom_col() +
      geom_text(aes(label = value), vjust = -0.5) +
      labs(title = "Daily Nutrient Targets", x = "", y = "Amount") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$treatmentAdvice <- renderUI({
    req(input$submit)
    
    conditions <- input$conditions
    if (length(conditions) == 0) {
      return(p("No specific conditions reported. Focus on general health improvement."))
    }
    
    tagList(
      lapply(conditions, function(cond) {
        advice <- get_treatment_advice(cond)
        div(
          class = "well",
          h4(cond),
          p(strong("Recommendations: "), advice$recommendations),
          p(strong("Medications: "), advice$medications),
          p(strong("When to see a doctor: "), advice$when_to_see_doctor)
        )
      })
    )
  })
  
  output$doctorTips <- renderUI({
    req(input$submit, user_data$stored_data)
    
    tips <- get_doctor_tips(user_data$stored_data)
    tagList(
      div(
        class = "well",
        h4("Professional Health Tips"),
        tags$ul(
          lapply(tips, function(tip) tags$li(tip))
        )
      )
    )
  })
  
  output$healthTips <- renderUI({
    req(input$submit, user_data$stored_data)
    
    tips <- get_health_tips(user_data$stored_data)
    tagList(
      div(
        class = "well",
        h4("Personalized Health Improvement Plan"),
        tags$ul(
          lapply(tips, function(tip) tags$li(tip))
        )
      )
    )
  })
  
  calculate_bmi_category <- function(bmi) {
    if (bmi < 18.5) return("Underweight")
    if (bmi < 25) return("Normal weight")
    if (bmi < 30) return("Overweight")
    return("Obese")
  }
  
  calculate_calorie_needs <- function(input) {
    if (input$gender == "Male") {
      bmr <- 88.362 + (13.397 * input$weight) + (4.799 * input$height) - (5.677 * input$age)
    } else {
      bmr <- 447.593 + (9.247 * input$weight) + (3.098 * input$height) - (4.330 * input$age)
    }
    
    activity_mult <- switch(input$activity,
                            "Sedentary" = 1.2,
                            "Lightly Active" = 1.375,
                            "Moderately Active" = 1.55,
                            "Very Active" = 1.725,
                            "Extremely Active" = 1.9)
    
    tdee <- bmr * activity_mult
    
    switch(input$health_goal,
           "Weight Loss" = tdee * 0.85,
           "Weight Gain" = tdee * 1.15,
           "Muscle Building" = tdee * 1.1,
           tdee)
  }
  
  generate_diet_plan <- function(user_data) {
    calorie_target <- user_data$calorie_needs
    
    protein_g <- ifelse(user_data$health_goal == "Muscle Building", 
                        user_data$weight * 2.2, 
                        user_data$weight * 1.6)
    protein_cal <- protein_g * 4
    fat_cal <- calorie_target * 0.25
    fat_g <- fat_cal / 9
    carb_cal <- calorie_target - protein_cal - fat_cal
    carb_g <- carb_cal / 4
    
    meals <- list(
      Breakfast = list(
        "Option 1" = "Oatmeal with berries and nuts + Greek yogurt",
        "Option 2" = "Whole grain toast with avocado + scrambled eggs"
      ),
      "Mid-Morning Snack" = list(
        "Option 1" = "Handful of almonds + apple",
        "Option 2" = "Protein shake with banana"
      ),
      Lunch = list(
        "Option 1" = "Grilled chicken with quinoa and steamed vegetables",
        "Option 2" = "Salmon with brown rice and salad"
      ),
      "Afternoon Snack" = list(
        "Option 1" = "Hummus with carrot sticks",
        "Option 2" = "Cottage cheese with pineapple"
      ),
      Dinner = list(
        "Option 1" = "Lean beef stir-fry with mixed vegetables",
        "Option 2" = "Tofu curry with lentils and spinach"
      )
    )
    
    if ("Diabetes" %in% user_data$conditions) {
      meals <- lapply(meals, function(meal) {
        lapply(meal, function(option) {
          paste(option, "(Low glycemic index version)")
        })
      })
    }
    
    if ("Hypertension" %in% user_data$conditions) {
      meals <- lapply(meals, function(meal) {
        lapply(meal, function(option) {
          paste(option, "(Low sodium version)")
        })
      })
    }
    
    meal_table <- data.frame(
      Meal = names(meals),
      Option_1 = sapply(meals, function(x) x[[1]]),
      Option_2 = sapply(meals, function(x) x[[2]])
    )
    
    nutrients <- data.frame(
      nutrient = c("Calories", "Protein (g)", "Carbs (g)", "Fat (g)"),
      value = round(c(calorie_target, protein_g, carb_g, fat_g))
    )
    
    list(table = meal_table, nutrients = nutrients)
  }
  
  get_treatment_advice <- function(condition) {
    switch(condition,
           "Diabetes" = list(
             recommendations = "Monitor blood sugar regularly. Engage in 30 minutes of moderate exercise daily. Focus on low glycemic index foods.",
             medications = "Metformin may be prescribed. Insulin therapy if needed.",
             when_to_see_doctor = "If blood sugar consistently above 180 mg/dL or below 70 mg/dL."
           ),
           "Hypertension" = list(
             recommendations = "Reduce sodium intake to less than 2,300 mg daily. Practice stress management techniques like meditation.",
             medications = "ACE inhibitors, beta-blockers, or diuretics may be prescribed.",
             when_to_see_doctor = "If blood pressure exceeds 140/90 mmHg regularly."
           ),
           "Heart Disease" = list(
             recommendations = "Follow a heart-healthy diet (Mediterranean style). Engage in supervised cardiac rehabilitation exercises.",
             medications = "Statins, beta-blockers, or aspirin therapy may be recommended.",
             when_to_see_doctor = "For chest pain, shortness of breath, or irregular heartbeat."
           ),
           "Arthritis" = list(
             recommendations = "Low-impact exercises like swimming or cycling. Maintain healthy weight to reduce joint stress.",
             medications = "NSAIDs or DMARDs may be prescribed. Topical pain relievers can help.",
             when_to_see_doctor = "For severe pain, joint deformity, or reduced mobility."
           ),
           "Asthma" = list(
             recommendations = "Avoid known triggers. Use air purifiers. Practice breathing exercises.",
             medications = "Inhalers (bronchodilators and corticosteroids) are common treatments.",
             when_to_see_doctor = "If using rescue inhaler more than twice a week or for severe attacks."
           ),
           "Thyroid" = list(
             recommendations = "Ensure adequate iodine intake. Regular blood tests to monitor levels.",
             medications = "Levothyroxine for hypothyroidism. Anti-thyroid drugs for hyperthyroidism.",
             when_to_see_doctor = "For unexplained weight changes, fatigue, or temperature sensitivity."
           )
    )
  }
  
  get_doctor_tips <- function(user_data) {
    tips <- c(
      "Stay hydrated - aim for at least 8 glasses of water daily.",
      "Get 7-9 hours of quality sleep each night.",
      "Practice good posture, especially if you sit for long periods."
    )
    
    if (user_data$age > 50) {
      tips <- c(tips,
                "Consider regular bone density tests and calcium supplements.",
                "Schedule annual vision and hearing check-ups.")
    } else if (user_data$age > 30) {
      tips <- c(tips,
                "Start regular health screenings (cholesterol, blood pressure).",
                "Focus on maintaining muscle mass with resistance training.")
    }
    
    if (user_data$gender == "Female") {
      tips <- c(tips,
                "Regular breast self-exams and mammograms as recommended.",
                "Ensure adequate iron intake, especially during menstruation.")
    } else if (user_data$gender == "Male") {
      tips <- c(tips,
                "Regular prostate health checks after age 50.",
                "Be mindful of heart health - men are at higher risk for cardiovascular disease.")
    }
    
    if (user_data$activity %in% c("Sedentary", "Lightly Active")) {
      tips <- c(tips,
                "Try to incorporate at least 30 minutes of moderate activity daily.",
                "Consider a standing desk or regular stretch breaks if you sit for work.")
    } else {
      tips <- c(tips,
                "Ensure proper warm-up and cool-down routines to prevent injury.",
                "Include rest days in your workout schedule for recovery.")
    }
    
    if ("Diabetes" %in% user_data$conditions) {
      tips <- c(tips,
                "Check your feet daily for any cuts or sores that may heal slowly.",
                "Carry a fast-acting glucose source in case of hypoglycemia.")
    }
    
    if ("Hypertension" %in% user_data$conditions) {
      tips <- c(tips,
                "Monitor your blood pressure at home and keep a log for your doctor.",
                "Limit caffeine and alcohol intake as they can affect blood pressure.")
    }
    
    sample(tips, min(10, length(tips)))
  }
  
  get_health_tips <- function(user_data) {
    tips <- c()
    
    if (user_data$bmi < 18.5) {
      tips <- c(tips,
                "Focus on nutrient-dense foods to gain weight healthily.",
                "Incorporate strength training to build muscle mass.")
    } else if (user_data$bmi > 25) {
      tips <- c(tips,
                "Gradual weight loss (1-2 lbs per week) is most sustainable.",
                "Increase fiber intake to feel fuller longer.")
    }
    
    if (user_data$activity %in% c("Sedentary", "Lightly Active")) {
      tips <- c(tips,
                "Start with 10-minute walks and gradually increase duration.",
                "Try desk exercises or short activity breaks every hour.")
    }
    
    tips <- c(tips,
              "Eat a variety of colorful fruits and vegetables for diverse nutrients.",
              "Practice mindfulness or meditation to reduce stress.",
              "Limit processed foods and added sugars.",
              "Build a consistent sleep schedule for better rest.",
              "Maintain social connections for mental health.")
    
    if ("Diabetes" %in% user_data$conditions) {
      tips <- c(tips,
                "Space carbohydrate intake evenly throughout the day.",
                "Pair carbs with protein or fat to slow glucose absorption.")
    }
    
    if ("Hypertension" %in% user_data$conditions) {
      tips <- c(tips,
                "Read food labels to monitor sodium content.",
                "Try potassium-rich foods like bananas and spinach to counter sodium effects.")
    }
    
    if ("Heart Disease" %in% user_data$conditions) {
      tips <- c(tips,
                "Increase omega-3 fatty acids from fish or flaxseeds.",
                "Avoid trans fats and limit saturated fats.")
    }
    
    sample(tips, min(8, length(tips)))
  }
}

shinyApp(ui = ui, server = server)
