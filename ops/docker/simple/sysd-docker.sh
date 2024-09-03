#!/usr/bin/env bash

cp dockertest.service /etc/systemd/system/

systemctl daemon-reload
systemctl enable docker
systemctl start docker
systemctl enable dockertest.service
systemctl start dockertest.service
systemctl status dockertest.service

