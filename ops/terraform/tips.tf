# tf is declarative, no loops or range
resource "aws_instance" "foo" {
  count         = 3
  ami           = "ami-2d39803a"
  instance_type = "t2.micro"

  availability_zone = "${element(var.azs, count.index)}" # put each instance in a different az

  tags {
    Name = "foo-${count.index}" # foo-0 through foo-2
  }
}

variable "azs" {
  description = "Run the EC2 Instances in these Availability Zones"
  type        = "list"
  default     = ["us-east-1a", "us-east-1b", "us-east-1c"]
}

output "ips" {
  value = ["${aws_instance.foo.*.public_ip}"] # wildcards work

  # value = ["${aws_instance.foo.0.public_ip}"] # so do indicies
}
