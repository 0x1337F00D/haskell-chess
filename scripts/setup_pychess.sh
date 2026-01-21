#!/bin/bash
if [ ! -d "pychess_repo" ]; then
    echo "Cloning pychess for benchmarking..."
    git clone https://github.com/pychess/pychess.git pychess_repo
else
    echo "pychess_repo already exists."
fi
