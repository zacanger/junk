#!/usr/bin/env node

const yearToCheck = '2021'
const exec = require('child_process').execSync

JSON.parse(exec('aws --profile prd ec2 describe-instances').toString())
  .Reservations.map((r) => r.Instances)
  .reduce((p, c) => p.concat(c), [])
  .filter((i) => i /* filter instances by attrs like i.Tags.Key/Value here */)
  .filter((i) => !i.Tags.filter((t) => t.Value === 'jane-ops').length)
  .map((i) => ({
    ip: i.PrivateIpAddress,
    url: '' /* construct or add url here, probably based on tags */,
  }))
  .map(({ ip, url }) => {
    let success = false
    try {
      success = exec(
        `echo -n | openssl s_client -connect ${ip}:443 -servername ${url} 2>/dev/null | openssl x509 -noout -dates`
      )
        .toString()
        .includes(yearToCheck)
    } catch {}
    return { ip, url, success }
  })
  .forEach((s) => {
    console.dir(s, { colors: true })
  })
