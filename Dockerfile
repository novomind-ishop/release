# Keep the existing Java version and complete JRE module set.
FROM eclipse-temurin:25.0.3_9-jre-jammy AS runtime-reference

RUN java --list-modules | sed 's/@.*//' > /tmp/jre-modules
# GnuPG is only needed to verify the Git repository key during the build.
RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl gnupg \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0xF911AB184317630C59970973E363C90F8F1B6217" \
      | gpg --batch --dearmor -o /etc/apt/keyrings/git-core-ppa.gpg \
    && gpg --batch --show-keys --with-colons /etc/apt/keyrings/git-core-ppa.gpg \
      | grep -q "fpr:::::::::F911AB184317630C59970973E363C90F8F1B6217:" \
    && rm -rf /var/lib/apt/lists/*

FROM eclipse-temurin:25.0.3_9-jdk-jammy AS runtime-build
COPY --from=runtime-reference /tmp/jre-modules /tmp/jre-modules
# Preserve all JRE modules, including providers loaded dynamically. Compress the
# runtime and omit debug data, headers, manuals and the default CDS archive.
RUN jlink \
      --add-modules "$(paste -sd, /tmp/jre-modules)" \
      --compress=zip-9 \
      --strip-debug \
      --no-header-files \
      --no-man-pages \
      --output /opt/java/runtime \
    && /opt/java/runtime/bin/java --list-modules | sed 's/@.*//' > /tmp/linked-modules \
    && cmp /tmp/jre-modules /tmp/linked-modules

FROM ubuntu:22.04

ARG GIT_VERSION="1:2.55.0-0ppa1~ubuntu22.04.2"

LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="ubuntu/22.04 Jammy Jellyfish"

ENV JAVA_HOME=/opt/java/openjdk
ENV PATH=$JAVA_HOME/bin:$PATH \
    JAVA_VERSION=jdk-25.0.3+9 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8 \
    RELEASE_DOCKER=true

COPY --from=runtime-build /opt/java/runtime/ /opt/java/openjdk/
# Retain Temurin's locale and optional system/custom CA certificate integration
# without installing the full locales package in the runtime image.
COPY --from=runtime-reference /usr/lib/locale/locale-archive /usr/lib/locale/locale-archive
COPY --from=runtime-reference /__cacert_entrypoint.sh /__cacert_entrypoint.sh
COPY --from=runtime-reference /etc/apt/keyrings/git-core-ppa.gpg /etc/apt/keyrings/git-core-ppa.gpg

# Bootstrap TLS trust from Ubuntu's repositories before enabling the HTTPS Git PPA.
RUN printf '%s\n' \
      'path-exclude=/usr/share/doc/*' \
      'path-include=/usr/share/doc/*/copyright' \
      'path-exclude=/usr/share/man/*' \
      'path-exclude=/usr/share/info/*' \
      > /etc/dpkg/dpkg.cfg.d/01-no-documentation \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends ca-certificates \
    && echo "deb [signed-by=/etc/apt/keyrings/git-core-ppa.gpg] https://ppa.launchpadcontent.net/git-core/ppa/ubuntu jammy main" \
      > /etc/apt/sources.list.d/git-core-ppa.list \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      curl tzdata bash fontconfig p11-kit \
      "git=${GIT_VERSION}" "git-man=${GIT_VERSION}" \
    && rm -rf /var/lib/apt/lists/* \
    && find "$JAVA_HOME/lib" -name '*.so' -exec dirname '{}' ';' \
      | sort -u > /etc/ld.so.conf.d/docker-openjdk.conf \
    && ldconfig \
    && test "$(git --version)" = "git version 2.55.0" \
    && curl --version \
    && java --version \
    && git config --global core.autocrlf input \
    && git config --global --add safe.directory "*"

COPY --chmod=755 release /root/release
COPY release.jar /root/release.jar
COPY target/git.HEAD /root/git.HEAD
RUN /root/release --help && /root/release --check-git

ENTRYPOINT ["/__cacert_entrypoint.sh"]
CMD ["/bin/bash"]
