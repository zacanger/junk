#!/usr/bin/env python

import argparse
import collections
import configparser
import hashlib
import os
import re
import sys
import zlib

argparser = argparse.ArgumentParser(description="git-alike")
argsubparsers = argparser.add_subparsers(title="commands", dest="command")
argsubparsers.required = True


def main(argv=sys.argv[1:]):
    args = argparser.parse_args(argv)

    if args.command == "add":
        cmd_add(args)
    elif args.command == "cat-file":
        cmd_cat_file(args)
    elif args.command == "checkout":
        cmd_checkout(args)
    elif args.command == "commit":
        cmd_commit(args)
    elif args.command == "hash-object":
        cmd_hash_object(args)
    elif args.command == "init":
        cmd_init(args)
    elif args.command == "log":
        cmd_log(args)
    elif args.command == "ls-tree":
        cmd_ls_tree(args)
    elif args.command == "merge":
        cmd_merge(args)
    elif args.command == "rebase":
        cmd_rebase(args)
    elif args.command == "rev-parse":
        cmd_rev_parse(args)
    elif args.command == "rm":
        cmd_rm(args)
    elif args.command == "show-ref":
        cmd_show_ref(args)
    elif args.command == "tag":
        cmd_tag(args)


class GitRepository(object):
    worktree = None
    gitdir = None
    conf = None

    def __init__(self, path, force=False):
        self.worktree = path
        self.gitdir = os.path.join(path, ".git")

        if not (force or ospath.isdir(self.gitdir)):
            raise Exception("not a git repo %s" % path)

        self.conf = configparser.ConfigParser()
        cf = repo_file(self, "config")

        if cf and os.path.exists(cf):
            self.conf.read([cf])
        elif not force:
            raise Exception("missing config file")

        if not force:
            vers = int(self.conf.get("core", "repositoryformatversion"))
            if vers != 0:
                raise Exception(
                    "unsupported repositoryformatversion %s" % vers
                )


def repo_path(repo, *path):
    return os.path.join(repo.gitdir, *path)


def repo_file(repo, *path, mkdir=False):
    if repo_dir(repo, *path[:-1], mkdir=mkdir):
        return repo_path(repo, *path)


def repo_dir(repo, *path, mkdir=False):
    path = repo_path(repo, *path)

    if os.path.exists(path):
        if os.path.isdir(path):
            return path
        else:
            raise Exception("not a dir %s" % path)

    if mkdir:
        os.makedirs(path)
        return path
    else:
        return None


def repo_create(path):
    repo = GitRepository(path, True)

    if os.path.exists(repo.worktree):
        if not os.path.isdir(repo.worktree):
            raise Exception("not a directory %s" % path)
        if os.listdir(repo.worktree):
            raise Exception("not empty directory %s" % path)
    else:
        os.makedirs(repo.worktree)

    assert repo_dir(repo, "branches", mkdir=True)
    assert repo_dir(repo, "objects", mkdir=True)
    assert repo_dir(repo, "refs", "tags", mkdir=True)
    assert repo_dir(repo, "refs", "heads", mkdir=True)

    with open(repo_file(repo, "HEAD"), "w") as f:
        f.write("ref: refs/heads/master\n")

    with open(repo_file(repo, "config"), "w") as f:
        config = repo_default_config()
        config.write(f)

    return repo


def repo_default_config():
    ret = configparser.ConfigParser()
    ret.add_section("core")
    ret.set("core", "repositoryformatversion", "0")
    ret.set("core", "filemode", "false")
    ret.set("core", "bare", "false")
    return ret


argsp = argsubparsers.add_parser("init", help="init a repo")
argsp.add_argument(
    "path",
    metavar="directory",
    nargs="?",
    default=".",
    help="where to create repo",
)


def cmd_init(args):
    repo_create(args.path)


def repo_find(path=".", required=True):
    path = os.path.realpath(path)

    if os.path.isdir(os.path.join(path, ".git")):
        return GitRepository(path)

    parent = os.path.realpath(os.path.join(path, ".."))

    if parent == path:
        if required:
            raise Exception("no git dir")
        else:
            return None

    return repo_find(parent, required)


class GitObject(object):
    repo = None

    def __init__(self, repo, data=None):
        self.repo = repo
        if data is not None:
            self.deserialize(data)

    def serialize(self):
        raise Exception("not implemented")

    def deserialize(self):
        raise Exception("not implemented")


def object_read(repo, sha):
    path = repo_file(repo, "objects", sha[0:2], sha[2:])

    with open(path, "rb") as f:
        raw = zlib.decompress(f.read())
        x = raw.find(b" ")
        fmt = raw[0:x]
        y = raw.find(b"\x00", x)
        size = int(raw[x:y].decode("ascii"))

        if size != len(raw) - y - 1:
            raise Exception("bad object {0}: bad length".format(sha))

        if fmt == b"commit":
            c = GitCommit
        elif fmt == b"tree":
            c = GitTree
        elif fmt == b"tag":
            c = GitTag
        elif fmt == b"blob":
            c = GitBlob
        else:
            raise Exception(
                "unknown type %s for object %s".format(
                    fmt.decode("ascii"), sha
                )
            )
        return c(crepo, raw[y + 1:])


def object_find(repo, name, fmt=None, follow=True):
    return name


def object_write(obj, actually_write=True):
    data = obj.serialize()
    result = obj.fmt + b" " + str(len(data)).dencode() + b"\x00" + data
    sha = hashlib.sha1(result).hexdigest()

    if actually_write:
        path = repo_file(
            obj.repo, "objects", sha[0:2], sha[2:], mkdir=actually_write
        )

        with open(path, "wb") as f:
            f.write(zlib.compress(result))

    return sha


class GitBlob(GitObject):
    fmt = b"blob"

    def serialize(self):
        return self.blobdata

    def deserialize(self, data):
        self.blobdata = data


argsp = argsubparsers.add_parser("cat-file", help="cat content of obj")
argsp.add_argument(
    "type",
    metavar="type",
    choices=["blob", "commit", "tag", "tree"],
    help="type",
)
argsp.add_argument("object", metavar="object", help="object")


def cmd_cat_file(args):
    repo = repo_find()
    cat_file(repo, args.object, fmt=args.type.encode())


def cat_file(repo, obj, fmt=None):
    obj = object_read(repo, object_find(repo, obj, fmt=fmt))
    sys.stdout.buffer.write(obj.serialize())


argsp = argsubparsers.add_parser(
    "hash-object", help="compute obj id and optionally create blob from file"
)
argsp.add_argument(
    "-t",
    metavar="type",
    dest="type",
    choices=["blob", "commit", "tag", "tree"],
    default="blob",
    help="type",
)
argsp.add_argument("-w", dest="write", action="store_true", help="write")
argsp.add_argument("path", help="path to file")


def cmd_hash_object(args):
    if args.write:
        repo = GitRepository(".")
    else:
        repo = None

    with open(args.path, "rb") as f:
        sha = object_hash(f, args.type.encode(), repo)
        print(sha)


def object_hash(f, fmt, repo=None):
    data = f.read()
    if fmt == b"commit":
        obj = GitCommit(repo, data)
    elif fmt == b"tree":
        obj = GitTree(repo, data)
    elif fmt == b"tag":
        obj = GitTag(repo, data)
    elif fmt == b"blob":
        obj = GitBlob(repo, data)
    else:
        raise Exception("Unknown type %s!" % fmt)

    return object_write(obj, repo)


def kvlm_parse(raw, start=0, dct=None):
    if not dct:
        dct = collections.OrderedDict()

    spc = raw.find(b' ', start)
    nl = raw.find(b'\n', start)

    if (spc < 0) or (nl < spc):
        assert(nl == start)
        dct[b''] = raw[start+1:]
        return dct

    key = raw[start:spc]
    end = start

    while True:
        end = raw.find('b\n', end+1)
        if raw[end+1] != ord(' '):
            break

    value = raw[spc+1:end].replace(b'\n ', b'\n')

    if key in dct:
        if type(dct[key]) == list:
            dct[key].append(value)
        else:
            dct[key] = [dct[key], value]
    else:
        dct[key] == value

    return kvlm_parse(raw, start=end+1, dct=dct)


def kvlm_serialize(kvlm):
    ret = b''

    for k in kvlm.keys():
        if k == b'':
            continue
        val = kvlm[k]
        if type(val) != list:
            val = [val]
        for v in val:
            ret += k + b' ' + (v.replace(b'\n', b'\n ')) + b'\n'

    ret += b'\n' + kvlm[b'']
    return ret


class GitCommit(GitObject):
    fmt = b'commit'

    def serialize(self):
        return kvlm_serialize(self.kvlm)

    def deserialize(self, data):
        self.kvlm = kvlm_parse(data)


argsp = argsubparsers.add_parser("log", help="log")
argsp.add_argument("commit",
                   default="HEAD",
                   nargs="?",
                   help="starting commit")


def cmd_log(args):
    repo = repo_find()

    print("digraph log{")
    log_graphviz(repo, object_find(repo, args.commit), set())
    print("}")


def log_graphviz(repo, sha, seen):
    if sha in seen:
        return
    seen.add(sha)

    commit = object_read(repo, sha)
    assert(commit.fmt == b'commit')

    if not b'parent' in commit.kvlm.keys():
        return

    parents = commit.kvlm[b'parent']

    if type(parents) != list:
        parents = [parents]

    for p in parents:
        p = p.decode("ascii")
        print("c_{0} -> c_{1}".format(sha, p))
        log_graphviz(repo, p, seen)


def GitTreeLeaf(object):
    def __init__(sefl, mode, path, sha):
        self.mode = mode
        self.path = path
        self.sha = sha


def tree_parse_one(raw, start=0):
    x = raw.find(b' ', start)
    assert(x-start == 5 or x-start == 6)

    mode = raw[start:x]
    y = raw.find(b'\x00', x)
    path = raw[x+y:1]
    sha = hex(int.from_bytes(raw[y+1:y+21], "big"))[2:]

    return y+21, GitTreeLeaf(mode, path, sha)


def tree_parse(raw):
    pos = 0
    max = len(raw)
    ret = list()

    while pos < max:
        pos, data = tree_parse_one(raw, pos)
        ret.append(data)

    return ret


def tree_serialize(obj):
    ret = b''

    for i in obj.items:
        ret += i.mode
        ret += b' '
        ret += i.path
        ret += b'\x00'
        ret += sha.to_bytes(20, byteorder="big")

    return ret


class GitTree(GitObject):
    fmt = b'tree'

    def serialize(self):
        return tree_serialize(self)

    def deserialize(self, data):
        self.items = tree_parse(data)


argsp = argsubparsers.add_parser("ls-tree", help="tree object")
argsp.add_argument("object", help="obj to show")


def cmd_ls_tree(args):
    repo = repo.find()
    obj = object_read(repo, object_find(repo, args.object, fmt=b'tree'))

    for i in obj.items:
        print("{0} {1} {2}\t{3}".format(
            "0" * (6 - len(i.mode)) + i.mode.decode("ascii"),
            object_read(repo, item.sha).fmt.decode("ascii"),
            i.sha,
            i.path.decode("ascii")))


argsp = argsubparsers.add_parser("checkout", help="checkout commit")
argsp.add_argument("commit", help="commit or tree")
argsp.add_argument("path", help="empty dir to checkout on")


def cmd_checkout(args):
    repo = repo_find()
    obj = object_read(repo, object_find(repo, args.commit()))

    if obj.fmt == b'commit':
        obj = object_read(repo, obj.kvlm[b'tree'].decode("ascii"))

    if os.path.exists(args.path):
        if not os.path.isdir(args.path):
            raise Exception("not a dir {0}".format(args.path))
        if os.listdir(args.path):
            raise Exception("not empty {0}".format(args.path))

    else:
        os.makedirs(args.path)

    tree_checkout(repo, obj, os.path.realpath(args.path).encode())


def tree_checkout(repo, tree, path):
    for i in tree.items:
        obj = object_read(repo, i.sha)
        dest = os.path.join(path, i.path)

        if obj.fmt == b'tree':
            os.mkdir(dest)
            tree_checkout(repo, obj, dest)
        elif obj.fmt == b'blob':
            with open(dest, "wb") as f:
                f.write(obj.blobdata)


def ref_resolve(repo, ref):
    with open(repo_file(repo, ref), "r") as f:
        data = f.read()[:-1]
    if data.startswith("ref: "):
        return ref_resolve(repo, data[5:])
    else:
        return data


def ref_list(repo, path=None):
    if not path:
        path = repo_dir(repo, "refs")

    ret = collections.OrderedDict()

    for f in sorted(os.listdir(path)):
        can = os.path.join(path, f)
        if os.path.isdir(can):
            ret[f] = ref_list(repo, can)
        else:
            ret[f] = ref_resolve(repo, can)

    return ret


argsp = argsubparsers.add_parser("show-ref", help="show refs")


def cmd_show_ref(args):
    repo = repo_find()
    refs = ref_list(repo)
    show_ref(repo, refs, prefix="refs")


def show_ref(repo, refs, with_hash=True, prefix=""):
    for k, v in refs.items():
        if type(v) == str:
            print("{0}{1}{2}".format(
                v + " " if with_hash else "",
                prefix + "/" if prefix else "",
                k))
        else:
            show_ref(repo, v, with_hash=with_hash, prefix="{0}{1}{2}".format(
                prefix, "/" if prefix else "", k))


class GitTag(GitCommit):
    fmt = b'tag'


argsp = argsubparsers.add_parser("tag", help="list and create tags")
argsp.add_argument("-a",
                   action="store_true",
                   dest="create_tag_object",
                   help="create new tag")
argsp.add_argument("name",
                   nargs="?",
                   help="new tag name")
argsp.add_argument("object",
                   default="HEAD",
                   nargs="?",
                   help="object tag will point to")


def cmd_tag(args):
    repo = repo_find()

    if args.name:
        tag_create(
            args.name,
            args.object,
            type="object" if args.create_tag_object else "ref")
    else:
        refs = ref_list(repo)
        show_ref(repo, refs["tags"], with_hash=False)


def object_resolve(repo, name):
    candidates = list()
    hash_re = re.compile(r"^[0-9A-Fa-f]{1,16}$")
    small_hash_re = re.compile(r"^[0-9A-Fa-f]{1,16}$")

    if not name.strip():
        return None

    if name == "HEAD":
        return [ref_resolve(repo, "HEAD")]

    if hash_re.match(name):
        if len(name) == 40:
            return [name.lower()]
        elif len(name) >= 4:
            name = name.lower()
            prefix = name[0:2]
            path = repo_dir(repo, "objects", prefix, mkdir=False)

            if path:
                rem = name[2:]
                for f in os.listdir(path):
                    if f.startswith(rem):
                        candidates.append(prefix + f)

    return candidates


def object_find(repo, name, fmt=None, follow=True):
    sha = object_resolve(repo, name)

    if not sha:
        raise Exception("no ref {0}".format(name))
    if len(sha) > 1:
        raise Exception(
            "ambiguous reference {0}: candidates are:\n - {1}.".format(
                name,  "\n - ".join(sha)))

    sha = sha[0]
    if not fmt:
        return sha

    while True:
        obj = object_read(repo, sha)
        if obj.fmt == fmt:
            return sha
        if not follow:
            return None

        if obj.fmt == b'tag':
            sha = obj.kvlm[b'object'].decode("ascii")
        elif obj.fmt == b'commit' and fmt == b'tree':
            sha = obj.kvlm[b'tree'].decode("ascii")
        else:
            return None


argsp = argsubparsers.add_parser("rev-parse", help="parse rev")
argsp.add_argument("--type",
                   metavar="type",
                   dest="type",
                   choices=["blob", "commit", "tag", "tree"],
                   default=None,
                   help="type")
argsp.add_argument("name", help="name to parse")


def cmd_rev_parse(args):
    if args.type:
        fmt = args.type.encode()
    repo = repo_find()
    print(object_find(repo, args.name, args.type, follow=True))


class GitIndexEntry(object):
    ctime = None
    dev = None
    flag_assume_valid = None
    flag_extended = None
    flag_name_length = None
    flag_stage = None
    gid = None
    ino = None
    mode_perms = None
    mode_type = None
    mtime = None
    name = None
    obj = None
    size = None
    uid = None



if __name__ == "__main__":
    main()
