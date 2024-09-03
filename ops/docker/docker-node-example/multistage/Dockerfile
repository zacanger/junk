FROM mhart/alpine-node:10 as base
RUN addgroup -g 1000 -S node && \
    adduser -u 1000 -S node -G node
WORKDIR /app

FROM base as deps
COPY --chown=node:node package*.json ./
RUN npm ci

FROM deps as test
WORKDIR /app
COPY --chown=node:node . /app
RUN npm t && \
  rm -rf node_modules && \
  npm ci --production

FROM test as release
WORKDIR /app
USER node
ENV NODE_ENV=production \
  TERM=linux \
  PORT=9999
EXPOSE 9999
HEALTHCHECK --interval=30s \
    --timeout=2s \
    --start-period=10s \
    --retries=10 \
  CMD node ${appdir}/healthcheck.js
CMD ["node", "."]
