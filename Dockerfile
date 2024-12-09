FROM eclipse-temurin:11.0.22_7-jre-jammy
COPY release /root/release
COPY release.jar /root/release.jar
ENV RELEASE_DOCKER=true
RUN chmod +x /root/release
RUN /root/release --help
RUN apt-get update && apt-get install -y dh-autoreconf libcurl4-openssl-dev libexpat1-dev \
      gettext libz-dev libssl-dev build-essential
RUN curl -L https://github.com/git/git/archive/refs/tags/v2.45.0.tar.gz > latest-git.tgz
RUN mkdir latest-git && tar -zxf latest-git.tgz -C latest-git --strip-components=1
RUN cd latest-git && ls -ltr && make configure && ./configure --prefix=/usr && make all
RUN cd latest-git && pwd && ls -l && pwd
RUN cd latest-git && make install
FROM eclipse-temurin:21.0.3_9-jre-jammy

# https://hub.docker.com/_/eclipse-temurin/tags
LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="ubuntu/22.04 Jammy Jellyfish"

RUN apt-get update && apt-get install -y tzdata bash
COPY --from=0 /latest-git/git /usr/bin/git
COPY --from=0 /usr/libexec/git-core /usr/libexec/git-core
RUN git --version
RUN curl --version
RUN git config --global core.autocrlf input && git config --global --add safe.directory "*"
COPY release /root/release
COPY release.jar /root/release.jar
COPY target/git.HEAD /root/git.HEAD
ENV RELEASE_DOCKER=true
RUN chmod +x /root/release
RUN /root/release --help
RUN /root/release --check-git

