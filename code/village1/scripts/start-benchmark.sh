#!/usr/bin/env bash

if [[ -z "$1" ]]
then
    echo "Missing benchmark class name"
    echo "Usage: start-benchmark.sh {package.BenchmarkClass} [parameters]"
    exit
fi


SSH_ENDPOINT="fknop@studssh.info.ucl.ac.be"

VILLAGE1_FOLDER="~/village1"
TEMPLATES_FOLDER="templates"
REMOTE_JAR="v1-benchmark.jar"
LOG_FILE="benchmark.log"

ssh ${SSH_ENDPOINT} "cd ${VILLAGE1_FOLDER}; \
                     java -cp ${REMOTE_JAR} $* \
                    &> ${LOG_FILE}&"