# Base image with R and Shiny Server
FROM rocker/shiny:latest

# Update system packages
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    libpython3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(\
  'shiny', 'dplyr', 'DT', 'ggplot2', 'patchwork', 'shinythemes', \
  'shinyWidgets', 'base64enc', 'readr', 'lubridate', \
  'gridExtra', 'zoo', 'reticulate'), repos='https://cloud.r-project.org')"

# Install remotes package for installing from GitHub
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"

# Install GeomMLBStadiums from GitHub
RUN R -e "remotes::install_github('bdilday/GeomMLBStadiums')"

# Set up Python virtual environment for reticulate
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Install Python dependencies
RUN pip install --upgrade pip && \
    pip install joblib==1.5.1 numpy==2.2.6 pandas==2.3.0 scikit-learn==1.7.0

# Set the working directory in the container
WORKDIR /srv/shiny-server/

# Copy your Shiny app to the image
COPY . .

# Expose Shiny port
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
