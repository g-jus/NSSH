# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("dplyr") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  #-----------------------------------------------------------------------------
  # Data preparation.
  #-----------------------------------------------------------------------------
  # Import of data.
  tar_target(herring_data, herring_read()),
  # Cleaning data.
  tar_target(clean_herring, cleaning_herring(herring_data)),

  #-----------------------------------------------------------------------------
  # Small reproducible sample for growth plot.
  #-----------------------------------------------------------------------------
  # Pre-sample data for growth model illustration.
  tar_target(
    growth_data_small, {
    set.seed(202603)
    dplyr::sample_n(clean_herring, min(5000, nrow(clean_herring)))
    }),

  #-----------------------------------------------------------------------------
  # Domain values for Shiny sliders/prediction grid.
  #-----------------------------------------------------------------------------
  # Maximum age for generating prediction grid
  tar_target(max_age, max(clean_herring$age, na.rm = TRUE)),

  #-----------------------------------------------------------------------------
  # Stats per year functions.
  #-----------------------------------------------------------------------------
  # Number of unique ID and ID numbers per year.
  tar_target(counts_per_year, count_per_year(clean_herring)),
  # Total weight (tonnes) per year.
  tar_target(weights_per_year, weight_per_year(clean_herring)),

  #-----------------------------------------------------------------------------
  # Age composition function.
  #-----------------------------------------------------------------------------
  # Age composition per year.
  tar_target(age_counts, age_count_for_year(clean_herring)),

  #-----------------------------------------------------------------------------
  # Mapping functions.
  #-----------------------------------------------------------------------------
  # Summary of fish per lat/lon and year.
  tar_target(catch_locations_all, location_catches_summary(clean_herring)),
  # Filtering locations for simpler map.
  tar_target(catch_locations_ocean, filter_ocean_points(catch_locations_all))

  #-----------------------------------------------------------------------------
  # QC (fail fast)
  #-----------------------------------------------------------------------------
  tar_target(qc_non_empty_clean, {
    if (nrow(clean_herring) == 0L) stop("QC: clean_herring has 0 rows."); TRUE
  }),
  tar_target(qc_age_ok, {
    if (!is.finite(max_age) || max_age < 1)
      stop("QC: max_age invalid or too small.")
    TRUE
  }),
  tar_target(qc_mapping_coords, {
    rng_lon <- range(catch_locations_ocean$lon, na.rm = TRUE)
    rng_lat <- range(catch_locations_ocean$lat, na.rm = TRUE)
    if (any(!is.finite(rng_lon)) || any(!is.finite(rng_lat))) {
      stop("QC: invalid lon/lat in catch_locations_ocean.")
    }
    TRUE
  })
)
