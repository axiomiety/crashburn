package main

import (
	"context"
	"encoding/json"
	"flag"
	"golang.org/x/time/rate"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
	"time"
)

func rateLogger(ctx context.Context, countChan <-chan struct{}) {
    ticker := time.NewTicker(time.Second)
    count := 0
    defer ticker.Stop()
    for {
        select {
        case <- ctx.Done():
            break
        case <- countChan:
            count += 1
        case <- ticker.C:
            log.Printf("requests/second: %d ", count);
            count = 0
        }
    }
}

func main() {
	var numWorkers = flag.Int("w", 20, "number of workers")
	var timeout = flag.Int("t", 5, "timeout, in seconds")
	flag.Parse()
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(*timeout)*time.Second)
	defer cancel()

	var wg sync.WaitGroup
    limiter := rate.NewLimiter(10, 15)
    countChan := make(chan struct{}, 100)
    go rateLogger(ctx, countChan)
    for i := 0; i < *numWorkers; i++ {

		go func(ctx context.Context, idx int, countChan chan<- struct{}) {
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
					if err := limiter.Wait(ctx); err != nil {
                        // if we get there, waiting would exceed the context's deadline
                        // so let's assume we're done
                        break
					}
					//log.Printf("worker[%d] fetching URL\n", idx)
					res, _ := client.Do(req)
					body, _ := ioutil.ReadAll(res.Body)
                    // we completed a request! let the counter know
                    countChan <- struct{}{}
					var data map[string]any
					_ = json.Unmarshal(body, &data)
					if val, ok := data["message"]; ok {
						log.Println(val)
					}
					defer res.Body.Close()
				}

			}
			log.Printf("coroutine exiting...")
		}(ctx, i, countChan)
	}
	wg.Wait()
	log.Println("done")
}
