FROM eclipse-temurin:17.0.8.1_1-jre-jammy
# https://hub.docker.com/_/eclipse-temurin/tags
LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="ubuntu/22.04 Jammy Jellyfish"

RUN apt-get update && apt-get install -y git tzdata bash
RUN git config --global core.autocrlf input
COPY release /root/release
COPY release.jar /root/release.jar
RUN chmod +x /root/release
RUN TERM=dumb /root/release --help
