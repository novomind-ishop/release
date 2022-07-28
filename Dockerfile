FROM nginx:1.17.2-alpine
LABEL maintainer="ishop-dev-infra@novomind.com" \
      novomind.deko.color="ec601a" \
      novomind.deko.linux.distribution="alpine/3.10"

RUN apk update && apk add tzdata && cp /usr/share/zoneinfo/Europe/Berlin /etc/localtime && echo "Europe/Berlin" > /etc/timezone && apk del tzdata \
    && echo -e "#!/bin/sh\nntpd -d -q -n -p pool.ntp.org" > /etc/periodic/daily/do-ntpd && chmod +x /etc/periodic/daily/do-ntpd
