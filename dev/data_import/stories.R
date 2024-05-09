## BUILD STORIES ###############################################################

build_stories <- function() {

  # Build empty table -------------------------------------------------------

  stories <- stories_empty_table()

  # Add every story ---------------------------------------------------------

  stories <- "xyz"


  # Create images and mapping -----------------------------------------------

  # IMPORTANT ON EVERY STORIES IMPORT!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  stories <- stories_map_images(stories = stories)


  # # Knit all stories Rmds ---------------------------------------------------
  #
  # library(here)
  # cc.buildr::stories_knit_all()


  # Return ------------------------------------------------------------------

  return(stories)

}
