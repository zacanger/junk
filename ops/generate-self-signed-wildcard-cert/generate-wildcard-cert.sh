#!/bin/sh

# you'll need a ca cert first, and a password

echo 'domain?'
read -r domain

sed "s/{{domainName}}/$domain/g" < wildcard-template.cnf > "$domain.cnf"

openssl genrsa -out "$domain.key.pem" 2048

openssl req \
  -config "$domain.cnf" \
  -new \
  -subj "/C=US/ST=FILLME/O=FILLME/OU=Domain Control Validated/CN=*.$domain" \
  -out "$domain.csr" \
  -key "$domain.key.pem"

openssl ca \
  -batch \
  -config "$domain.cnf" \
  -keyfile FILLME.key.pem \
  -cert FILLME.crt \
  -in "$domain.csr" \
  -out "$domain.crt" \
  -extensions v3_req

openssl pkcs12 \
  -export \
  -in "$domain.crt" \
  -inkey "$domain.key.pem" \
  -out "$domain.pfx"

rm "$domain.cnf"
rm "$domain.csr"
