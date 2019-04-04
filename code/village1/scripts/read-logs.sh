#!/usr/bin/env bash

SSH_ENDPOINT="fknop@studssh.info.ucl.ac.be"

VILLAGE1_FOLDER="~/village1"

ssh ${SSH_ENDPOINT} "cd ${VILLAGE1_FOLDER};
                     cat benchmark.log"