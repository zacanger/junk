Playing around with Terraform. Notes:

* Check out `terraform import`
  * Only imports state, can't generate files (yet)
* Use `terraform fmt` to validate and format files
* `terraform plan -out=thing.plan`, then `terraform apply thing.plan`
  * Ignore `*.plan` files, they'll have interpolated keys and whatnot
* Probably use S3 for state files
* Don't try to run from a subdirectory, run from root (root doesn't need to mean
  project root, it could be `project/prd` or whatever)
* How would we manage resources that are also managed by something else (like
  how many instances are in an ASG right now?)
* Might make sense to add a `terraform=true` tag to all TF resources until we're
  used to it.
* Absolutely do not mix dev and prd state files!
