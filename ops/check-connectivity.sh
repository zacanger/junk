#!/usr/bin/env bash

# check-connectivity.sh foo.com 443
echo -n | telnet "$1" "$2"
