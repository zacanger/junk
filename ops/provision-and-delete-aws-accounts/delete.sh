#!/usr/bin/env bash
set -e

# shellcheck disable=SC1090
source <(aws sts assume-role \
  --role-arn "$arn_to_assume" \
  --role-session-name "Something"  |
  jq -r  '.Credentials | @sh "export AWS_SESSION_TOKEN=\(.SessionToken)\nexport AWS_ACCESS_KEY_ID=\(.AccessKeyId)\nexport AWS_SECRET_ACCESS_KEY=\(.SecretAccessKey) "')

username=$(aws iam list-users | jq -r '.Users[] | select (.UserName | startswith("AA_")) | .UserName')

if [ -n "$username" ]; then
  # Can't use a for loop, because $username is not a list,
  # it's potentially a multi-line string
  echo "$username" | while read u; do
    echo "Deleting $u"
    aws iam remove-user-from-group --group-name Administrators --user-name "$u"
    aws iam delete-login-profile --user-name "$u"
    aws iam delete-user --user-name "$u"
  done
fi

# shellcheck disable=SC1090
source <(env | awk -F= '/AWS/ {print "unset ", $1}')
