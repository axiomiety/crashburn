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
			c.Key = string(value.([]byte))
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
		buff, err := reader.ReadBytes('e')
		check(err)
		val, err := strconv.Atoi(string(buff[:len(buff)-1]))
		check(err)
		container.add(val)
	case 'l':
		return parse(Container{List: make([]interface{}, 0)}, reader)
	case 'd':
		return parse(Container{Dict: make(map[string]interface{})}, reader)
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		buff, err := reader.ReadBytes(':')
		check(err)
		strLen := string(b)
		if len(buff) > 1 {
			strLen += string(buff[:len(buff)-1])
		}
		strLenInt, err := strconv.Atoi(strLen)
		check(err)
		val := make([]byte, 0)
		for i := 0; i < strLenInt; i++ {
			b, err = reader.ReadByte()
			val = append(val, b)
		}
		container.add(val)
		return parse(container, reader)
	}
	return container
}

func ParseBencoded(r io.Reader) interface{} {
	reader := bufio.NewReader(r)

	container := parse(Container{}, reader)
	fmt.Printf("%v\n", container)
	return container.obj()
}
