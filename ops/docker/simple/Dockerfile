FROM node:6.2.0
ADD package.json /opt/test/package.json
RUN cd /opt/test && npm install
ADD . /opt/test
EXPOSE 9000
CMD ["node", "/opt/test"]

