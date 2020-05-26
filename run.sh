#! /bin/bash

make main
yaws --runmod start --conf ./yaws.conf
