package main

import (
//	"go/parser"
//	"go/printer"
//	"go/token"
	"os"
	"os/exec"
)

func compile(filename string) {
	// find gofmt
	gofmt, err := exec.LookPath("gofmt")
	if err != nil {
		panic(err)
	}
	fmt := exec.Command(gofmt)
	fmt.Stderr = os.Stderr
	fmt.Stdout = os.Stdout
	wc, err := fmt.StdinPipe()
	if err != nil {
		panic(err)
	}

	// find guile
	guile, err := exec.LookPath("guile")
	if err != nil {
		panic(err)
	}
	script := os.Getenv("GOPATH") + "/src/gos2go/scm/gos2go-guile.scm"
	cmd := exec.Command(guile, "--debug", "-s", script, filename)
	cmd.Stdin = os.Stdin
	cmd.Stderr = os.Stderr
	cmd.Stdout = wc

	err = fmt.Start()
	if err != nil { panic(err) }
	err = cmd.Run()
	if err != nil { panic(err) }
	err = wc.Close()
	if err != nil { panic(err) }
	err = fmt.Wait()
	if err != nil { panic(err) }
	
}

func main() {
	filename := os.Args[1]
	compile(filename)
}