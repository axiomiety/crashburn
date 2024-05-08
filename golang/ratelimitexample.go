package main

import (
    "net/http"
    "context"
    "log"
<<<<<<< HEAD
=======
    "fmt"
>>>>>>> c3149bf ([golang] playing with rate limits)
    "sync"
    "time"
    "io/ioutil"
)

func main() {
<<<<<<< HEAD
    ctx, cancel := context.WithTimeout(context.Background(),2*time.Second)
    defer cancel()

    var wg sync.WaitGroup
    for i:=0;i<4;i++ {
=======
    ctx, cancel := context.WithTimeout(context.Background(),10*time.Second)
    defer cancel()

    var wg sync.WaitGroup
    for i:=0;i<10;i++ {
>>>>>>> c3149bf ([golang] playing with rate limits)
        wg.Add(1)
        go func(ctx context.Context, idx int) {
            defer wg.Done()
            client := &http.Client{}
            req, _ := http.NewRequest("GET", "https://api.exchange.coinbase.com/currencies", nil)
            req.Header.Add("Content-Type", "application/json")
            
            for {
                select {
                case <- ctx.Done():
<<<<<<< HEAD
                    break
=======
                    log.Printf("coroutine %d exiting...", i)
                    return
>>>>>>> c3149bf ([golang] playing with rate limits)
                default:
                    log.Printf("worker[%d] fetching URL\n", i)
                    res, _ := client.Do(req)
                    body,_ := ioutil.ReadAll(res.Body)
<<<<<<< HEAD
                    print(string(body)[:32])
=======
                    fmt.Printf("%s\n", string(body)[:32])
>>>>>>> c3149bf ([golang] playing with rate limits)
                    defer res.Body.Close()
                }

            }
<<<<<<< HEAD
            log.Printf("coroutine exiting...")
=======
>>>>>>> c3149bf ([golang] playing with rate limits)
        }(ctx, i)
    }
    wg.Wait()
    log.Println("done")
}
