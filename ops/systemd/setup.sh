#!/usr/bin/env bash

cp /root/test/test1.service /etc/systemd/system
cp /root/test/test2.service /etc/systemd/system

systemctl daemon-reload

systemctl enable test1.service
systemctl enable test2.service

systemctl start test1.service
systemctl start test2.service

systemctl status test1.service
systemctl status test2.service

echo "
server {
    listen 80;
    server_name one.zacanger.com;
    location / {
        proxy_pass http://127.0.0.1:9999;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
}

server {
    listen 80;
    server_name two.zacanger.com;
    location / {
        proxy_pass http://127.0.0.1:9900;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
}
" >> /etc/nginx/sites-enabled/default

service nginx restart

