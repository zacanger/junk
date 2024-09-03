# ubuntu 18.04

# as root
# main packages
apt-get update
apt-get upgrade -y
curl -sLO \
  https://storage.googleapis.com/kubernetes-release/release/`curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt`/bin/linux/amd64/kubectl && \
  chmod +x ./kubectl && mv ./kubectl /usr/local/bin/kubectl
curl -fsSL \
  https://download.docker.com/linux/ubuntu/gpg | apt-key add - && \
  echo "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable" >> /etc/apt/sources.list && \
  curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg > /tmp/dkey && \
  apt-key add /tmp/dkey

apt-get update && \
  apt-get -y install \
  apt-transport-https \
  build-essential \
  ca-certificates \
  curl \
  docker-ce \
  gettext-base \
  git-lfs \
  gnupg2 \
  jq \
  libc6 \
  libgcc1 \
  libgssapi-krb5-2 \
  libicu60 \
  libssl1.1 \
  libstdc++6 \
  openjdk-11-jdk \
  python3-pip \
  resolvconf \
  software-properties-common \
  sshpass \
  tzdata \
  zip \
  zlib1g

# dotnet and related tools
# add more versions of the sdk here if needed
wget https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
  dpkg -i packages-microsoft-prod.deb && \
  apt-get update && apt-get install -y dotnet-sdk-2.2 dotnet-sdk-3.1 && \
  dotnet

pip3 install --no-cache-dir awscli pycodestyle

# nodejs
apt-get install -y nodejs && \
  curl -SsL https://registry.npmjs.org/npm/-/npm-6.0.0.tgz | tar -xzf - && \
  cd package && \
  make install && \
  npm i -g n && \
  n 12 && \
  n prune && \
  npm i -g npm && \
  node -v && npm -v && cd .. && rm -rf package

# Clean up docker images
curl -L https://raw.githubusercontent.com/spotify/docker-gc/master/docker-gc -o /usr/sbin/docker-gc
chmod a+rx /usr/sbin/docker-gc
echo '#!/bin/bash
/usr/sbin/docker-gc
' > /etc/cron.daily/docker-gc
chmod a+rx /etc/cron.daily/docker-gc

# set up jenkins user
useradd -m -s /bin/bash jenkins
usermod -aG docker jenkins

# su jenkins
# copy-paste the script from the jenkins ui to ~/jenkins.sh
# get the agent.jar from the jenkins ui
chmod +x jenkins.sh
nohup ./jenkins.sh &
