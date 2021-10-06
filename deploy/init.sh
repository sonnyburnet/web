#!/bin/sh -eu

export ekg_datadir=home/nix/deploy/ekg
cd /home/nix/

sleep 2
echo 'launch server..'
./bin/edgenode-server --cfgPath deploy/config.yaml --pathToKatip deploy --pathToJwk deploy +RTS -N -AL32m -n4m -A32m -qb0 -I0 -T -s
