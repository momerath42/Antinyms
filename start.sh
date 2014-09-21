#!/bin/bash

erl -env ERL_FULLSWEEP_AFTER 0 +P 300000 +A 4 -smp auto -s antinyms -sname antinyms
