package data

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strconv"
)

func parseInt(data []byte) int {
	i, err := strconv.Atoi(string(data))
	check(err)
	return i
}

func readUntilE(r *bufio.Reader) []byte {
	buff := make([]byte, 4098)
	offset := 0
	for {
		b, err := r.ReadByte()
		check(err)
		switch b {
		case 'e':
			return buff[:offset]
		default:
			buff[offset] = b
			offset += 1
		}
	}
}

type Container struct {
	List []interface{}
	Dict map[string]interface{}
	Key  string
	Val  interface{}
}

func (c *Container) add(value interface{}) {
	if c.List != nil {
		c.List = append(c.List, value)
	} else if c.Dict != nil {
		if c.Key == "" {
			c.Key = value.(string)
		} else {
			c.Dict[c.Key] = value
			c.Key = ""
		}
	} else {
		c.Val = value
	}
}

func (c *Container) obj() interface{} {
	if c.List != nil {
		return c.List
	} else if c.Dict != nil {
		return c.Dict
	} else if c.Val != nil {
		return c.Val
	} else {
		panic("don't know what to return!")
	}
}

func parse(container Container, reader *bufio.Reader) Container {
	b, err := reader.ReadByte()
	if err != nil {
		log.Printf("end reached")
		return container
	}
	switch b {
	case 'e':
		return container
	case 'i':
		val, err := strconv.Atoi(string(readUntilE(reader)))
		check(err)
		container.add(val)
	case 'l':
		return parse(Container{List: make([]interface{}, 0)}, reader)

	}
	return container
}

func ParseBencoded(r io.Reader) interface{} {
	reader := bufio.NewReader(r)

	container := parse(Container{}, reader)
	fmt.Printf("%v\n", container)
	return container.obj()
}
