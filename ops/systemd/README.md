just trying some things out

* put the server under /opt/testserver
* put the service file under /etc/systemd/system
* do a `systemctl enable testserver.service`
  * this should come back with a 'created symlink' etc. message
* `systemctl start testserver.service` to start it!

* to check, `systemctl status testserver.service`
  * this should come back with a bit of info on that service

* if changing anything (user/group of the service, for example):
  * `systemctl daemon-reload`
  * `systemctl restart testserver.service`
  * then maybe check it with `status` to be sure

* `systemd-analyze verify <unit>` can check for problems with a config

* **IMPORTANT**
  * systemd does not check for things in `$PATH`
  * MUST specify full path, eg `/bin/sh`

* log-checking (as root/with sudo):
  * `journalctl -u <unit>`
  * `journalctl -u <unit> -f` (like `tail -f`)

* default type is simple (`Type=simple`) under `[Service]`
  * there's also forking, oneshot, dbus, notify, and idle
  * with simple, the process listed under ExecStart is the
    main process for that service
  * with forking, it becomes a child proc, and the parent exits
    after startup.

