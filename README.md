WorkstackR: Personal Productivity & Project Tracking
A high-performance R Shiny dashboard designed for senior data scientists to manage complex workstreams, track blockers, and visualize project timelines.

Features
Interactive Data Entry: Uses rhandsontable and DT for seamless, Excel-like ticket updates.

Dynamic Timelines: Interactive Gantt-style visualizations powered by timevis.

Workflow Tracking: Manage review dates, next steps, and blockers.

Enhanced UX: Utilizes shinyWidgets for custom inputs and shinyalert for stylized confirmations.

Performance: Loading states managed by shinycustomloader for a smooth analytical experience.

Tech Stack
Core Framework: shiny, shinydashboard

Data Wrangling: dplyr, lubridate

Interactive Tables: rhandsontable, DT

Visualizations: timevis

UI/UX Enhancements: shinycustomloader, shinyWidgets, shinyalert

Installation & Usage
Clone the repository:

Bash

git clone https://github.com/your-username/workstack-r.git
Install dependencies: Open R Studio and run:

R

install.packages(c("dplyr", "lubridate", "shinycustomloader", "timevis", 
                   "rhandsontable", "DT", "shinyWidgets", "shinyalert", 
                   "shiny", "shinydashboard"))
Run the App:

R

shiny::runApp()