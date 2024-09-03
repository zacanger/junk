This is a test using Docker and systemd together.

```
docker build -t dockertest .
docker run -d dockertest -p 9000:9000
docker start dockertest
docker ps
docker exec -it <container id from previous step> /bin/bash
```

