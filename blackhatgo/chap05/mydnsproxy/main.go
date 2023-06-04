package main

import (
	"bufio"
	"flag"
	"github.com/miekg/dns"
	"golang.org/x/exp/slog"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"
)

func localproxy(w dns.ResponseWriter, req *dns.Msg) {
	var resp dns.Msg
	resp.SetReply(req)
	for _, q := range req.Question {
		logger.Info("sinkhole request", "domain", q.Name)
		a := dns.A{
			Hdr: dns.RR_Header{
				Name:   q.Name,
				Rrtype: dns.TypeA,
				Class:  dns.ClassINET,
				Ttl:    10,
			},
			A: net.ParseIP("127.0.0.1").To4(),
		}
		resp.Answer = append(resp.Answer, &a)
	}
	w.WriteMsg(&resp)
}

type DnsForwardFunc func(w dns.ResponseWriter, req *dns.Msg)

type holder struct {
	Filename    string
	Domains     []string
	ForwardFunc DnsForwardFunc
}

func (data *holder) reload() {
	// remove everything we currently have
	for _, domain := range data.Domains {
		dns.HandleRemove(domain)
	}

	// forward everything by default
	dns.HandleFunc(".", data.ForwardFunc)

	readFile, err := os.Open(data.Filename)
	defer readFile.Close()

	if err != nil {
		log.Fatal(err)
	}

	fileScanner := bufio.NewScanner(readFile)
	fileScanner.Split(bufio.ScanLines)
	for fileScanner.Scan() {
		domain := fileScanner.Text()
		dns.HandleFunc(domain, localproxy)
		data.Domains = append(data.Domains, domain)
	}
	logger.Info("config reloaded", "domains", len(data.Domains))
}

var logger = slog.New(slog.NewJSONHandler(os.Stdout, nil))

func main() {
	serverAddr := flag.String("dns", "8.8.8.8:53", "<ip>:<port>")
	domains := flag.String("domains", "domains.txt", "file containing a list of domains to sinkhole, one per line")
	flag.Parse()

	logger.Info("starting", "pid", os.Getpid(), "dns", *serverAddr, "domains", *domains)
	go func() {
		log.Fatal(dns.ListenAndServe(":53", "udp", nil))
	}()

	sigsStop := make(chan os.Signal, 1)
	signal.Notify(sigsStop, syscall.SIGINT, syscall.SIGTERM)
	sigHup := make(chan os.Signal, 1)
	signal.Notify(sigHup, syscall.SIGHUP)
	done := make(chan bool, 1)

	forward := func(w dns.ResponseWriter, req *dns.Msg) {
		resp, err := dns.Exchange(req, *serverAddr)
		if err != nil {
			dns.HandleFailed(w, req)
			return
		}
		if err := w.WriteMsg(resp); err != nil {
			dns.HandleFailed(w, req)
			return
		}
		for _, q := range req.Question {
			logger.Info("forward req", "domain", q.Name)
		}
	}

	data := holder{Filename: *domains, ForwardFunc: forward}
	go func() {
		data.reload()
		for {
			<-sigHup
			data.reload()
		}
	}()

	go func() {
		<-sigsStop
		logger.Info("signal received, shutting down")
		done <- true
	}()

	<-done
}
