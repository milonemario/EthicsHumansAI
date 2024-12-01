# Create image
# > podman build -t aiethics .
# > docker build -t aiethics .

# Create container
# > podman create -it --name aiethics --network host -p 8888:8888 -v ./:/project --privileged --user root aiethics
# > docker create -it --name aiethics --network host -p 8888:8888 -v ./:/project --privileged --user root aiethics

# On non-linux docker
# > docker create -it --name aiethics -p 8888:8888 -v ./:/project --privileged --user root aiethics 

# Temporary: To run rstudio for development
# > podman create -ti --replace --name rstudio_aiethics -v ./:/home/rstudio/workspace -p 8787:8787 --privileged -e DISABLE_AUTH=true rocker/rstudio:4.4
# > docker create -ti --name rstudio_aiethics -v ./:/home/rstudio/workspace -p 8787:8787 --privileged -e DISABLE_AUTH=true rocker/rstudio:4.4


FROM debian:bookworm

ARG jupyter_password="llm"

USER root
RUN mkdir /project/
WORKDIR /project/

ENV DEBIAN_FRONTEND=noninteractive

# Python
#RUN apt-get update && apt-get install -y --no-install-recommends build-essential python3-full python3-pip python3-setuptools python3-dev
RUN apt-get update && apt-get install -y --no-install-recommends python3-full python3-pip

# R
RUN apt-get install -y r-base-dev
# Packages needed for tidyverse
RUN apt-get install -y libcurl4-openssl-dev libssl-dev \
  libxml2-dev libfontconfig1-dev libharfbuzz-dev \
  libfribidi-dev libfreetype6-dev \
  libpng-dev libtiff5-dev libjpeg-dev
# Package needed for ggpattern
#RUN apt-get install -y libudunits2-dev \
#  libproj-dev libgdal-dev

# Latex
RUN apt-get install -y texlive-latex-recommended texlive-latex-extra

# Cleanup to reduce image size
RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/*

# Python packages
COPY ./python/requirements.txt requirements.txt

RUN pip3 install --break-system-packages --upgrade pip
RUN pip3 install --break-system-packages --no-cache-dir --upgrade -r requirements.txt
RUN rm requirements.txt

# Install jupyter lab (can be removed for replication package)
RUN pip3 install --break-system-packages jupyterlab

# Config jupyter lab
RUN mkdir /root/.jupyter/
RUN echo c.ServerApp.ip = \'0.0.0.0\' > /root/.jupyter/jupyter_lab_config.py
RUN echo from jupyter_server.auth import passwd >> /root/.jupyter/jupyter_lab_config.py
RUN echo password = passwd\(\"${jupyter_password}\"\) >> /root/.jupyter/jupyter_lab_config.py
RUN echo c.ServerApp.password = password >> /root/.jupyter/jupyter_lab_config.py
RUN echo c.ServerApp.allow_root=True >> /root/.jupyter/jupyter_lab_config.py

# R packages
# RUN R -e "install.packages('remotes')"
# 
# RUN R -e "remotes::install_version('arrow', version='16.1.0', repos='https://cloud.r-project.org')"
# RUN R -e "remotes::install_version('lfe', version='3.0-0', repos='https://cloud.r-project.org')"
