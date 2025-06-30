#!/bin/bash
# setup.sh - Version compatible Render
apt-get update -qq
apt-get install -y --no-install-recommends \
    r-base \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev
