#!/bin/sh -eu

export ekg_datadir=home/nix/deploy/ekg
cd /home/nix/

sleep 2
echo 'launch server..'
./bin/server --cfg_path deploy/config.yaml --path_to_katip deploy --path_to_jwk deploy --cfg_admin_storage_path deploy/admin_storage --migration ./migration
