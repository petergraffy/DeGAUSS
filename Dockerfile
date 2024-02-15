# Load R image
FROM r-base:latest

# Make a directory in the container
RUN mkdir /home/r-environment

# Install R dependencies
RUN R -e "install.packages(c('daymetr', 'tidyverse', 'terra', 'gtools', 'data.table', 'future', 'furrr', 'progressr', 'remotes'))"
RUN R --quiet -e "remotes::install_github('degauss-org/dht')"

# Set environment description for dht
ENV degauss_description="daymet climate variables"

# Copy our R script to the container
COPY entrypoint.R /home/r-environment/entrypoint.R

# Copy over the loyalty_geocoded.csv to container
ADD loyalty_geocoded.csv /home/r-environment/loyalty_geocoded.csv

# Run the container
CMD R -e "source('/home/r-environment/entrypoint.R')"
