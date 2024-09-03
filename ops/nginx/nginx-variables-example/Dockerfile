FROM nginx:stable-alpine-perl
RUN apk add --update curl && \
    rm /etc/nginx/conf.d/default.conf && \
    mkdir -p /web && \
    rm -rf /var/cache/apk/*
COPY nginx.conf /etc/nginx/nginx.conf
COPY config.conf /etc/nginx/sites-enabled/config.conf
WORKDIR /etc/nginx
CMD ["nginx", "-g", "daemon off;"]
