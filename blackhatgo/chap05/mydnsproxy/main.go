package main

import (
	"bufio"
	"github.com/miekg/dns"
	"golang.org/x/exp/slog"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"
)

func forward(w dns.ResponseWriter, req *dns.Msg) {
	var serverAddr = "8.8.8.8:53"
	log.Printf("%+v\n", req)
	resp, err := dns.Exchange(req, serverAddr)
	if err != nil {
		dns.HandleFailed(w, req)
		return
	}
	if err := w.WriteMsg(resp); err != nil {
		dns.HandleFailed(w, req)
		return
	}
}

func localproxy(w dns.ResponseWriter, req *dns.Msg) {
	var resp dns.Msg
	resp.SetReply(req)
	for _, q := range req.Question {
		log.Printf("sinkholing req for %s", q.Name)
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

type holder struct {
	Filename string
	Domains  []string
}

func (data *holder) reload() {
	// remove everything we currently have
	for _, domain := range data.Domains {
		dns.HandleRemove(domain)
	}

	// forward everything by default
	dns.HandleFunc(".", forward)

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
	log.Printf("reloaded %d domains", len(data.Domains))
}

func main() {
	logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))
	logger.Info("starting", "pid", os.Getpid())
	go func() {
		log.Fatal(dns.ListenAndServe(":53", "udp", nil))
	}()

	sigsStop := make(chan os.Signal, 1)
	signal.Notify(sigsStop, syscall.SIGINT, syscall.SIGTERM)
	sigHup := make(chan os.Signal, 1)
	signal.Notify(sigHup, syscall.SIGHUP)
	done := make(chan bool, 1)

	data := holder{Filename: "domains.txt"}
	go func() {
		data.reload()
		for {
			<-sigHup
			data.reload()
		}
	}()

	go func() {
		<-sigsStop
		log.Printf("shutting down")
		done <- true
	}()

	<-done
}
