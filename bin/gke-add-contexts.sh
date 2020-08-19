i#!/bin/sh

gcloud container clusters get-credentials gke-us-central1 --zone us-central1-a --project tulip-infra
gcloud container clusters get-credentials gke-us-east1 --region us-east1 --project tulip-prod-na
gcloud container clusters get-credentials gke-us-east1 --region us-east1 --project tulip-staging
gcloud container clusters get-credentials gke-us-east1 --region us-east1 --project tulip-develop
gcloud container clusters get-credentials gke-us-east1 --region us-east1 --project tulip-playground
gcloud container clusters get-credentials gke-europe-west2 --region europe-west2 --project tulip-prod-eu
