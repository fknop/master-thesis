#!/usr/bin/env bash

if [[ -z "$1" ]]
then
    echo "Missing benchmark class name"
    echo "Usage: start-benchmark.sh {package.BenchmarkClass} {logfile} [parameters]"
    exit
fi

arg=$2
if [[ -z "$2" || "${arg:${#arg}-4}" != ".log" ]]
then
    echo "Missing or wrong log file"
    echo "Usage: start-benchmark.sh {package.BenchmarkClass} {logfile}.log [parameters]"
    exit
fi



SSH_ENDPOINT="fknop@studssh.info.ucl.ac.be"
JABBA_ENDPOINT="fknop@jabba.info.ucl.ac.be"

VILLAGE1_FOLDER="~/village1"
TEMPLATES_FOLDER="templates"
REMOTE_JAR="v1-benchmark.jar"

CLASS_NAME=$1
LOG_FILE=$2
shift 2

ssh ${SSH_ENDPOINT} "ssh ${JABBA_ENDPOINT} \
                     'cd ${VILLAGE1_FOLDER}; \
                     java -cp ${REMOTE_JAR} ${CLASS_NAME} $* \
                     &> ${LOG_FILE}& ' "