#!/usr/bin/env bash

# Script for the xfce4-genmon-plugin
# obtain the info about the process consuming most of the CPU
cmd=$(ps ax -o %cpu,pid,comm --sort=-%cpu | head -n 2 | tail -1)
# echo ${cmd}
cpu=$(grep --only-matching -e "[-+]\?[0-9]*\.\?[0-9]\+" <<< ${cmd} | head -n 1)
pid=$(grep --only-matching -e "[-+]\?[0-9]*\.\?[0-9]\+" <<< ${cmd} | tail -n 1)
comm=$(grep --only-matching -e "\w*" <<< ${cmd} | tail -n 1)
# echo "<txt>"$(ps a -o comm --sort=-%cpu | head -n 2 | tail -1)"</txt>"
echo "<txt>"${comm}"</txt>"
echo "<tool>"${comm}": "${cpu}"% CPU, pid: "${pid}"</tool>"
