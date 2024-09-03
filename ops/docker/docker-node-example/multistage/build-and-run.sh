#!/bin/sh

commit_hash=`git rev-parse --short=10 HEAD 2>/dev/null`
image_name="zacanger/multistage-node:$commit_hash"
docker build -t "$image_name" .
docker run -d -p 9999:9999 "$image_name"
