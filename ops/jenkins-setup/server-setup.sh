#!/usr/bin/env bash

# debian buster

# Distributed Builds plugins
/usr/local/bin/install-plugins.sh ssh-slaves

# install Notifications and Publishing plugins
/usr/local/bin/install-plugins.sh email-ext
/usr/local/bin/install-plugins.sh mailer
/usr/local/bin/install-plugins.sh slack

# Artifacts
/usr/local/bin/install-plugins.sh htmlpublisher

# Pipeline install
/usr/local/bin/install-plugins.sh \
    ace-editor \
    ant \
    antisamy-markup-formatter \
    apache-httpcomponents-client-4-api \
    authentication-tokens \
    aws-java-sdk \
    bouncycastle-api \
    branch-api \
    build-timeout \
    cloudbees-folder \
    change-assembly-version-plugin \
    command-launcher \
    credentials \
    credentials-binding \
    display-url-api \
    docker-commons \
    docker-workflow \
    durable-task \
    git \
    github \
    github-api \
    github-branch-source \
    gradle \
    handlebars \
    hashicorp-vault-plugin \
    jackson2-api \
    jdk-tool \
    jquery-detached \
    jsch \
    junit \
    last-changes \
    ldap \
    lockable-resources \
    mapdb-api \
    matrix-auth \
    matrix-project \
    momentjs \
    msbuild \
    mstest \
    nuget \
    nunit \
    pam-auth \
    pipeline-build-step \
    pipeline-github-lib \
    pipeline-graph-analysis \
    pipeline-input-step \
    pipeline-milestone-step \
    pipeline-model-api \
    pipeline-model-declarative-agent \
    pipeline-model-definition \
    pipeline-model-extensions \
    pipeline-rest-api \
    pipeline-stage-step \
    pipeline-stage-tags-metadata \
    pipeline-stage-view \
    pipeline-utility-steps \
    pipeline-aws \
    plain-credentials \
    resource-disposer \
    scm-api \
    script-security \
    ssh-agent \
    ssh-credentials \
    structs \
    subversion \
    test-results-analyzer \
    timestamper \
    token-macro \
    workflow-aggregator \
    workflow-api \
    workflow-basic-steps \
    workflow-cps \
    workflow-cps-global-lib \
    workflow-durable-task-step \
    workflow-job \
    workflow-multibranch \
    workflow-scm-step \
    workflow-step-api \
    workflow-support

apt-get update && \
      apt-get -y install \
      apt-transport-https \
      ca-certificates \
      curl \
      gettext-base \
      gnupg2 \
      jq \
      tzdata \
      vim \
      zip

# su jenkins
