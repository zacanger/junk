Oh look, it's Daniel Falabelabala. And we're upstairs, which would've been nice to know.

This lecture will be on the virtues and how-tos of AWS. Well, S3, anyway. Basically, since Mongo can't store images without doing difficult-ish things involving GridFS, the prescription is to use S3 in... basically the same way, actually, which results in more total requests, but the hope is that Amazon's caching or somesuch is good enough to offset the latency.

Amazon needs your regional server to be passed in during requests. Which is both sensible and silly, since really that should be a part of your keys.

When Amazon wants a 'Key' in your paramss, that's not a key. That's the actual thing you're ughin' with. so, req.body.img.

Buffers in node are bytes in python are strings in PHP if they were mutable.


