package main

import (
	"context"
	"io/ioutil"
	"log"
    "encoding/json"
	"net/http"
	"sync"
	"time"
    "flag"
)

func main() {
    var numWorkers = flag.Int("w", 20, "number of workers")
    flag.Parse()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	var wg sync.WaitGroup
	for i := 0; i < *numWorkers; i++ {

		go func(ctx context.Context, idx int) {
		    wg.Add(1)
			defer wg.Done()
			client := &http.Client{}
			req, _ := http.NewRequest("GET", "https://api.exchange.coinbase.com/products/ETH-USD/book?level=2", nil)
			req.Header.Add("Content-Type", "application/json")

			for {
				select {
				case <-ctx.Done():
					return
				default:
					log.Printf("worker[%d] fetching URL\n", idx)
					res, _ := client.Do(req)
					body, _ := ioutil.ReadAll(res.Body)
                    var data map[string]any
                    _ = json.Unmarshal(body, &data)
                    if val, ok := data["message"]; ok {
                        log.Println(val)
                    }
					defer res.Body.Close()
				}

			}
			log.Printf("coroutine exiting...")
		}(ctx, i)
	}
	wg.Wait()
	log.Println("done")
}
