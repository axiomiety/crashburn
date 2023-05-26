package main

import (
	"fmt"
	"os"
	"errors"
	"flag"
	"github.com/miekg/dns"
)

type result struct {
	IPAddress string
	Hostname  string
}

func lookupA(fqdn, serverAddr string) ([]string, error) {
	var m dns.Msg
	var ips []string
	m.SetQuestion(dns.Fqdn(fqdn), dns.TypeA)
	in, err := dns.Exchange(&m, serverAddr)
	if err != nil {
		return ips, err
	}
	if len(in.Answer) < 1 {
		return ips, errors.New("no answer")
	}
	for _, answer := range in.Answer {
		if a, ok := answer.(*dns.A); ok {
			ips = append(ips, a.A.String())
		}
	}
	return ips, nil
}

func lookupCNAME(fqdn, serverAddr string) ([]string, error) {
	var m dns.Msg
	var fqdns []string
	m.SetQuestion(dns.Fqdn(fqdn), dns.TypeCNAME)
	in, err := dns.Exchange(&m, serverAddr)
	if err != nil {
		return fqdns, err
	}
	if len(in.Answer) < 1 {
		return fqdns, errors.New("no answer")
	}
	for _, answer := range in.Answer {
		if c, ok := answer.(*dns.CNAME); ok {
			fqdns = append(fqdns, c.Target)
		}
	}

	return fqdns, nil
}

func main() {
	var (
		flDomain      = flag.String("domain", "", "the domain to perform guesses against")
		flWordlist    = flag.String("wordlist", "", "the wordlist used for guessing")
		flWorkerCount = flag.Int("c", 100, "the number of workers to use")
		flServerAddr  = flag.String("server", "8.8.8.8:53", "the DNS server to use")
	)
	flag.Parse()

	if *flDomain == "" || *flWordlist == "" {
		fmt.Println("-domain and -wordlist are required")
		os.Exit(1)
	}
	fmt.Println(*flWorkerCount, *flServerAddr)
}
