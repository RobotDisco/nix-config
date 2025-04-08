#!/usr/bin/env zsh
# setup-kubectl-contexts.sh -- Script to set up all of my Kubernetes clusters

# Gimme a list of all my GCP projects
typeset -A gcp_projects
gcp_projects[tulip-infra]=us-east1
gcp_projects[tulip-playground]=us-east1
gcp_projects[tulip-develop]=us-east1
gcp_projects[tulip-staging]=us-east1
gcp_projects[tulip-prod-de]=europe-west3
gcp_projects[tulip-prod-eu]=europe-west2
gcp_projects[tulip-prod-na]=us-east1

for env in ${(k)gcp_projects}; do
    local region=$gcp_projects[$env];
    #echo gcloud config set project $env
    kubectl config delete-context $env
    gcloud container clusters get-credentials gke-${region} --region ${region} --project $env
    kubectl config rename-context gke_${env}_${region}_gke-${region} $env
done

okta-aws-cli --profile awscn -z
AWS_PROFILE=awscn aws eks update-kubeconfig --name eks-cn-north-1 --alias tulip-prod-cn
