#!/usr/bin/env bash
set -e

# fill in the variables and copy out all the images you want to copy into a file list.txt

old_account_profile=''
new_account_profile=''
old_account_number=''
new_account_number=''

for image in $(cat list.txt); do
  $(aws --profile $old_account_profile ecr get-login --no-include-email --region us-east-1)
  docker pull "$image"
  new_image=$(echo "$image" | gsed "s/$old_account_number/$new_account_number/")
  docker tag "$image" "$new_image"
  $(aws --profile $new_account_profile ecr get-login --no-include-email --region us-east-1)
  docker push "$new_image"
done

# this will delete _all_ your local images, so comment it out if you don't need it.
# it makes more sense to delete once at the end than to delete after each image,
# because cached layers speed this up significantly.
docker rmi -f $(docker images -q)
