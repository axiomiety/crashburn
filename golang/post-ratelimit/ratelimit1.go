package main

import (
	"encoding/json"
	"flag"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
)

func main() {
	var numWorkers = flag.Int("w", 20, "number of workers")
	flag.Parse()

	var wg sync.WaitGroup
	for i := 0; i < *numWorkers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			client := &http.Client{}
			req, _ := http.NewRequest("GET", "https://api.exchange.coinbase.com/products/ETH-USD/book?level=2", nil)
			req.Header.Add("Content-Type", "application/json")
			for {
				res, _ := client.Do(req)
				body, _ := ioutil.ReadAll(res.Body)
				defer res.Body.Close()
				var data map[string]any
				_ = json.Unmarshal(body, &data)
				if val, ok := data["message"]; ok {
					log.Println(val)
				}
			}
		}()
	}
	wg.Wait()
	log.Println("done")
}
