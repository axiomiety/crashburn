package main

import (
    "net/http"
    "context"
    "log"
    "sync"
    "time"
    "io/ioutil"
)

func main() {
    ctx, cancel := context.WithTimeout(context.Background(),2*time.Second)
    defer cancel()

    var wg sync.WaitGroup
    for i:=0;i<4;i++ {
        wg.Add(1)
        go func(ctx context.Context, idx int) {
            defer wg.Done()
            client := &http.Client{}
            req, _ := http.NewRequest("GET", "https://api.exchange.coinbase.com/currencies", nil)
            req.Header.Add("Content-Type", "application/json")
            
            for {
                select {
                case <- ctx.Done():
                    break
                default:
                    log.Printf("worker[%d] fetching URL\n", i)
                    res, _ := client.Do(req)
                    body,_ := ioutil.ReadAll(res.Body)
                    print(string(body)[:32])
                    defer res.Body.Close()
                }

            }
            log.Printf("coroutine exiting...")
        }(ctx, i)
    }
    wg.Wait()
    log.Println("done")
}
