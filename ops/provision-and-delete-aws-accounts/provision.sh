#!/usr/bin/env bash
set -e

source <(aws sts assume-role \
  --role-arn "$arn_to_assume" \
  --role-session-name "ProvisionTemporaryUser"  |
  jq -r  '.Credentials | @sh "export AWS_SESSION_TOKEN=\(.SessionToken)\nexport AWS_ACCESS_KEY_ID=\(.AccessKeyId)\nexport AWS_SECRET_ACCESS_KEY=\(.SecretAccessKey) "')

username=$(date | md5sum | head -c 16)
username="AA_$username"
password=$(date | md5sum | tail -c 16 | tr ' ' '_')
password="$password"

# Create the user
aws iam create-user \
  --user-name "$username" \
  --tags 'Key=temporary,Value=true' "Key=expire_at,Value=$expire_at" \
  --permissions-boundary "$policy_to_add"

aws iam create-login-profile \
  --user-name "$username" \
  --password "$password"

aws iam add-user-to-group \
  --group-name Administrators \
  --user-name "$username"

echo "Username: $username"

# shellcheck disable=SC1090
source <(env | awk -F= '/AWS/ {print "unset ", $1}')
