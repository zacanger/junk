terraform {
  required_version = ">= 0.12.6"
}

provider "aws" {
  version = ">= 2.28.1"
  region  = var.region
}

data "aws_eks_cluster" "cluster" {
  name = module.eks.cluster_id
}

data "aws_eks_cluster_auth" "cluster" {
  name = module.eks.cluster_id
}

provider "kubernetes" {
  host = data.aws_eks_cluster.cluster.endpoint
  cluster_ca_certificate = base64decode(
    data.aws_eks_cluster.cluster.certificate_authority.0.data
  )
  token            = data.aws_eks_cluster_auth.cluster.token
  load_config_file = false
  version          = "~> 1.11"
}

module "eks" {
  source          = "terraform-aws-modules/eks/aws"
  cluster_name    = var.cluster_name
  subnets         = var.private_subnets
  cluster_version = var.cluster_version

  tags = {
    Name = var.cluster_name
  }

  vpc_id = var.vpc_id

  node_groups_defaults = {
    ami_type  = "AL2_x86_64"
    disk_size = 60
  }

  node_groups = {
    nodes = {
      desired_capacity = 6
      max_capacity     = 30
      min_capacity     = 6

      instance_type = "t3.2xlarge"
      k8s_labels = {
        Cluster = var.cluster_name
      }
      additional_tags = {
        Name = "${var.cluster_name}-node"
      }
    }
  }

  map_roles    = var.map_roles
  map_users    = var.map_users
  map_accounts = var.map_accounts
}
