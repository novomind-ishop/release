FROM eclipse-temurin:11.0.22_7-jre-jammy
COPY release /root/release
COPY release.jar /root/release.jar
ENV RELEASE_DOCKER=true
RUN chmod +x /root/release
RUN /root/release --help
RUN apt-get update && apt-get install -y dh-autoreconf libcurl4-gnutls-dev libexpat1-dev \
      gettext libz-dev libssl-dev build-essential libcurl4-openssl-dev
RUN curl -L https://github.com/git/git/archive/refs/tags/v2.44.0.tar.gz > latest-git.tgz
RUN mkdir latest-git && tar -zxf latest-git.tgz -C latest-git --strip-components=1
RUN cd latest-git && ls -ltr && make configure && ./configure --prefix=/usr && make all
RUN cd latest-git && pwd && ls -l && pwd
FROM eclipse-temurin:17.0.8.1_1-jre-jammy

# https://hub.docker.com/_/eclipse-temurin/tags
LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="ubuntu/22.04 Jammy Jellyfish"

RUN apt-get update && apt-get install -y tzdata bash
COPY --from=0 /latest-git/git /usr/bin/git
RUN git --version
RUN git config --global core.autocrlf input
COPY release /root/release
COPY release.jar /root/release.jar
ENV RELEASE_DOCKER=true
RUN chmod +x /root/release
RUN /root/release --help
RUN /root/release --check-git
