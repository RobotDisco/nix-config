#!/usr/bin/env bash

docker run -it -v $HOME/.aws:/root/.aws -v $PWD:/app -v $(dirname $SSH_AUTH_SOCK):$(dirname $SSH_AUTH_SOCK) -e SSH_AUTH_SOCK=$SSH_AUTH_SOCK -v $HOME/.config/gcloud:/root/.config/gcloud gcr.io/tulip-infra/terraform:0.12.26 bash
