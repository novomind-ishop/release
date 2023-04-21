FROM eclipse-temurin:17.0.3_7-jre-alpine
LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="alpine/3.10"

RUN apk update && apk add git tzdata bash && cp /usr/share/zoneinfo/Europe/Berlin /etc/localtime && echo "Europe/Berlin" > /etc/timezone && apk del tzdata \
    && echo -e "#!/bin/sh\nntpd -d -q -n -p pool.ntp.org" > /etc/periodic/daily/do-ntpd && chmod +x /etc/periodic/daily/do-ntpd
RUN git config --global core.autocrlf input
COPY release /root/release
COPY release.jar /root/release.jar
RUN chmod +x /root/release
RUN /root/release --help
