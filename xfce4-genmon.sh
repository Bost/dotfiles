#!/usr/bin/env bash

# xfce4-genmon-plugin
# only the process name of the process using most of the CPU
echo "<txt>"$(ps a -o comm --sort=-%cpu | head -n 2 | tail -1)"</txt>"
# process name, pid and short command of the process using most of the CPU
echo "<tool>"$(ps a -o %cpu,pid,comm --sort=-%cpu | head -n 2 | tail -1)"</tool>"
