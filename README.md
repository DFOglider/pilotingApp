# pilotingApp

UNDER DEVELOPMENT

This is a Shiny app that is intended to be used by pilots of
[SeaExplorer](https://www.alseamar-alcen.com/products/underwater-glider/seaexplorer)
gliders. The app downloads real-time data transmitted from the glider
over Iridium, processes it into a format suitable for plotting, and
then displays plots of the various fields of interest.

## How to use:

1. Install [R](www.r-project.org) and [RStudio](www.rstudio.com)

2. Download the Shiny app code from this repository, either through
   direct download, using Github Desktop, or by typing:
   ```
   $ git clone https://github.com/melanybelzile/pilotingApp.git
   ```
   in a terminal.

   The latter is recommended, as then updating the app can be done
   with a simple `git` command or by pulling changes in Github Desktop.

3. Open the file `app.R` in Rstudio, and click the "Run App"
   button. You will need to install several packages (as listed at the
   top of `app.R`). To do this run:
   ```r
   install.packages(c('shiny', 'oce', 'ocedata', 'measurements', 'leaflet', 'RCurl', 'geosphere', 'XML'))
   ```
   in the R terminal before clicking "Run App".


