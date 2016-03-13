# ShinyHastings Readme

This is the repository for the Shiny app "ShinyHastings", which was designed to let users experience the influence of proposal types on the Metropolis Hastings algorithm. It was designed by Zarah Weiß in the course of the seminar "Bayesian data analysis & cognitive modeling 2015" in the winter term 2015/16.

## Repository Content

The repository contains the following files:
* **README.md**, this readme file
* **shiny-hastings.pdf**, a paper describing theoretical background and implementational details to ShinyHasitings
* **/shiny-hastings**, the directory containing the Shiny code to run the program
* **/shiny-hastings/server.R**, the file containing the server side of the program
* **/shiny-hastings/ui.R**, the file containing the user interface of the program
* **/shiny-hastings/helper.R**, the helper file containing the actual implementation of the MH algorithms

## ShinyHastings Description & Usage

This app was created to show the influence of proposal types on the Metropolis Hastings algorithm.
It consists of two panels: a settings panel to the left or at the top of the page (depending on the browser size), and a result panel displaying the summary statistics and plots to investigate the algorithm's performance to the right or at the bottom of the page.

Each panel has separate tabs, further structuring the application based on functionality. While the result panel displays a separate tab for each of the offered statistics and convergence measures, the 

In order to start a sampling process, the "Approximate target!" button needs to be clicked.


In the 'main' tab, you can choose between different proposal types and adjust their parameters. Click 'Aproximate!'
#                                to start the algorithm. Caution: this will take a while!"),
#                              
#                              p("Please note: When opened for the first time, the program will run for a while to perform the first analysis,
#                                before you can make your own adjustments!"),
#                              p("In the 'settings' tab, you may also adjust:"),
#                              p("1) The target distribution to approximate and the sampling distributions to approximate with.
#                                Note: in order to achieve reasonable results, they should be similar."),
#                              p("2) The iteration set-up. Note: more iterations and chains increase sampling accuracy as well as sampling duration.
#                                Also, the number of burn-in iterations cannot exceed the number of iterations. If it does, the program stops."),
#                              p("For further instructions and implementational details, please consult the ",
#                                a("Github project page", href = "https://github.com/zweiss/ShinyHastings"
