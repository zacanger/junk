package main

import (
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"syscall"
)

// Following along with this talk: <https://www.youtube.com/watch?v=MHv6cWjvQjM>

func main() {
	switch os.Args[1] {
	case "run":
		run()
	case "child":
		child()
	default:
		panic("oh no")
	}
}

// ./thing run cmd args
func run() {
	cmd := exec.Command("/proc/self/exe", append([]string{"child"}, os.Args[2:]...)...)

	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	cmd.SysProcAttr = &syscall.SysProcAttr{
		Cloneflags:   syscall.CLONE_NEWUTS | syscall.CLONE_NEWPID | syscall.CLONE_NEWNS,
		Unshareflags: syscall.CLONE_NEWNS,
	}

	handleErr(cmd.Run())
}

func child() {
	cg()

	cmd := exec.Command(os.Args[2], os.Args[3:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	handleErr(syscall.Sethostname([]byte("box")))
	handleErr(syscall.Chroot("/tmp/box"))
	handleErr(os.Chdir("/"))
	handleErr(syscall.Mount("proc", "proc", "proc", 0, ""))
	handleErr(syscall.Mount("thing", "tmp", "tmpfs", 0, ""))
	handleErr(cmd.Run())
	handleErr(syscall.Unmount("proc", 0))
	handleErr(syscall.Unmount("thing", 0))
}

func cg() {
	cgroups := "/sys/fs/cgroup/"
	pids := filepath.Join(cgroups, "pids")
	os.Mkdir(filepath.Join(pids, "box"), 0755)

	handleErr(ioutil.WriteFile(filepath.Join(pids, "box/pids.max"), []byte("20"), 0700))
	handleErr(ioutil.WriteFile(filepath.Join(pids, "box/notify_on_release"), []byte("1"), 0700))
	handleErr(ioutil.WriteFile(filepath.Join(pids, "box/cgroup.procs"), []byte(strconv.Itoa(os.Getpid())), 0700))
}

func handleErr(err error) {
	if err != nil {
		panic(err)
	}
}
