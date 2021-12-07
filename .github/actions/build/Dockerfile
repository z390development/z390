# Container image that runs your code
FROM eclipse-temurin:8

# install git so we can get version label
RUN apt -y update;apt -y install git zip python3.8-venv libsdl-pango-dev

# Create a virtual env with required packages installed
RUN python -m venv /opt/docenv
COPY requirements.txt /opt/requirements.txt
RUN /opt/docenv/bin/pip install wheel
RUN /opt/docenv/bin/pip install -r /opt/requirements.txt

# Copies your code file from your action repository to the filesystem path `/` of the container
COPY entrypoint.sh /entrypoint.sh

# Code file to execute when the docker container starts up (`entrypoint.sh`)
ENTRYPOINT ["/entrypoint.sh"]