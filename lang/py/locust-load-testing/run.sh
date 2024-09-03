#!/usr/bin/env bash

locust \
  -f locustfile.py \
  --no-web \
  -c 1000 \
  -r 100 \
  -H $HOST \
  -t 15s \
  --csv=results
