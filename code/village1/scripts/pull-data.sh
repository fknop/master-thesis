#!/usr/bin/env bash


# TO BE STARTED FROM PROJECT ROOT

if [[ -z "$1" ]]
then
    echo "Usage: pull-data.sh {local-data-folder}"
    exit
fi

SSH_ENDPOINT="fknop@studssh.info.ucl.ac.be"

VILLAGE1_FOLDER="~/village1"
DATA_FOLDER="data"

scp -r ${SSH_ENDPOINT}:${VILLAGE1_FOLDER}/${DATA_FOLDER} $1
