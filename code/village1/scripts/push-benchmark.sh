#!/usr/bin/env bash


# TO BE STARTED FROM PROJECT ROOT

SCALA_VERSION="2.12"

BENCHMARK_JAR="./v1-benchmark/target/scala-${SCALA_VERSION}/v1-benchmark.jar"

SSH_ENDPOINT="fknop@studssh.info.ucl.ac.be"

VILLAGE1_FOLDER="~/village1"
TEMPLATES_FOLDER="templates"

scp templates/* ${SSH_ENDPOINT}:${VILLAGE1_FOLDER}/${TEMPLATES_FOLDER}
scp ${BENCHMARK_JAR} ${SSH_ENDPOINT}:${VILLAGE1_FOLDER}