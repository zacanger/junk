Simple example Dockerized Go web service that returns info on Loona members in JSON.

```
docker build -t something . # build the containers, including compiling the app
docker run -d -p 4000:4000 something # run the service on port 400
curl localhost:4000 | jq . # get some loona member info
```
