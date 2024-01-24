systemPackagesUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(4,
             box(
               title = "System Packages",
               status = "primary",
               width = NULL,
               solidHeader = TRUE,
               includeMarkdown("## Computing Environments of Norbert, Kuba, and Mateusz

Norbert is a tech enthusiast who seamlessly navigates between his MacBook running MacOS and a computer powered by CachyOS, an Arch Linux-based distribution. This dual setup allows him to enjoy the best of both worlds.

On the other hand, Kuba has a preference for Pop OS, a user-friendly Linux distribution. Pop OS is known for its simplicity and focus on productivity, making it an ideal choice for Kuba's computing needs.

Mateusz, on the other hand, has opted for Ubuntu, a popular and widely-used Linux distribution that strikes a balance between ease of use and robust features.

### Package Managers:

- **MacOS (on MacBook):** Norbert manages software packages using the native package manager and tools provided by MacOS. Additionally, he utilizes Homebrew, a popular third-party package manager for MacOS, to easily install and manage a wide range of software.

- **CachyOS (Arch Linux-based):** Norbert relies on the powerful Pacman package manager, which is central to the Arch Linux ecosystem. Pacman simplifies package management and ensures a streamlined experience.

- **Pop OS:** Kuba benefits from the apt package management system, which is also used in Ubuntu. Apt is a reliable and efficient package manager that simplifies the installation and maintenance of software.

- **Ubuntu:** Mateusz utilizes the Ubuntu Software Center and apt package manager. Ubuntu's package management system is well-established, offering a vast repository of software for easy installation and updates.

These diverse computing environments showcase the flexibility of operating systems and package managers, allowing users to tailor their setups according to their preferences and requirements.
")
             )
             
            
      ),
    
    
    
      column(4,
      box(title = "Plot of Number of Packages installed in logarithmic scale",width = NULL,
          status = "success",
          solidHeader = TRUE,
          plotlyOutput(ns("plot1")) %>% withSpinner()
      )
      )
  
    
    ,
  
    column(4, box(
      title = "Input",
      status = "warning",
      width = NULL,
      collapsible = TRUE,
      solidHeader = TRUE,
      selectInput(ns("player"), 
                  "Select the person for the system packages to analyze:",
                  c("Norbert - Linux" = "norlinux",
                    "Norbert - MacOS" = "normacos",
                    "Mateusz" = "mat",
                    "Kuba"= "kuba"),
                  selected = "norlinux"),
      numericInput(ns("ile"), "How many common prefixes of packages to show:",  min = 3,
                   max = 12,
                   value = 6,
                   step = 1),
      numericInput(ns("prefixy"), "The length of the prefix:",  min = 1,
                   max = 10,
                   value = 3,
                   step = 1)
      
    ),
    box(
      title = "Plot of Prefixes in packages",
      status = "success",
      width = NULL,
      solidHeader = TRUE,
      plotlyOutput(ns("plot2")) %>% withSpinner()
    )
         
  
  
  )
  
  )
  )
  
}
