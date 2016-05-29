#!/bin/bash
COUNTER=0
while [ $COUNTER -le 25 ]; do
	echo "pinging 10.1.1.$COUNTER"
	ping -c1 10.1.1.$COUNTER
	let COUNTER=COUNTER+1
done
