FROM node:16.13.1-buster AS base

WORKDIR /app
RUN chown node:node /app
USER node

COPY --chown=node:node package.json /app/package.json
COPY --chown=node:node package-lock.json /app/package-lock.json

RUN npm install

COPY --chown=node:node / /app

FROM base as dev

FROM base as prod

CMD ["npm", "start"]
