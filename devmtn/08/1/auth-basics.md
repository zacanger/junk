## Security

Basic HTTP:
- restricted based on system user/pass
- http://zacanger:encryptedpassword@zacanger.com

Form-based:
- restricted based on cookies
- success = stored cookie on client
- `POST whatever.com/login (headers)`

Token-based:
- requests with auth token
- https://foo.bar/whatever?auth_token=asd;lkfjasdfj348fj

OAuth
- rate limited, expired, revoked server-side

Passport
- this is, clearly, the preferred method, both with devmtn and with express in general, i feel
