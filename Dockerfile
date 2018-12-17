# Pull base image.
FROM ubuntu:16.04
# Install.
RUN \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get install -y curl
  
# Add files.
WORKDIR /home
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . .

# Define default command.
ENTRYPOINT ["sh", "-c","/home/curl-runnings -f /home/tests.yml"]
