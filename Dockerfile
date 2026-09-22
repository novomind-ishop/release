FROM eclipse-temurin:25.0.3_9-jre-jammy

ARG GIT_VERSION="1:2.55.0-0ppa1~ubuntu22.04.2"

# https://hub.docker.com/_/eclipse-temurin/tags
LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="ubuntu/22.04 Jammy Jellyfish"

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl gnupg \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xF911AB184317630C59970973E363C90F8F1B6217" \
      | gpg --batch --dearmor -o /etc/apt/keyrings/git-core-ppa.gpg \
    && gpg --batch --show-keys --with-colons /etc/apt/keyrings/git-core-ppa.gpg \
      | grep -q "fpr:::::::::F911AB184317630C59970973E363C90F8F1B6217:" \
    && echo "deb [signed-by=/etc/apt/keyrings/git-core-ppa.gpg] https://ppa.launchpadcontent.net/git-core/ppa/ubuntu jammy main" \
      > /etc/apt/sources.list.d/git-core-ppa.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends tzdata bash \
      "git=${GIT_VERSION}" "git-man=${GIT_VERSION}" \
    && rm -rf /var/lib/apt/lists/*
RUN test "$(git --version)" = "git version 2.55.0"
RUN curl --version
RUN git config --global core.autocrlf input && git config --global --add safe.directory "*"
COPY release /root/release
COPY release.jar /root/release.jar
COPY target/git.HEAD /root/git.HEAD
ENV RELEASE_DOCKER=true
RUN chmod +x /root/release
RUN /root/release --help
RUN /root/release --check-git
