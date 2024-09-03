FROM golang:1.10.3 as compiler
WORKDIR /app
COPY main.go .
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o app .

FROM alpine:latest
RUN apk --no-cache add ca-certificates
WORKDIR /app
COPY --from=compiler /app .
COPY members.json .
EXPOSE 3000
CMD ["./app"]
