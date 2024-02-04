package data

import (
	"bufio"
	"fmt"
	"io"
	"reflect"
	"strconv"
)

type Holder interface {
	Add(value interface{})
	Obj() interface{}
	Empty() bool
}

type ListHolder struct {
	List []interface{}
}

type DictHolder struct {
	Dict map[string]interface{}
	// tracks the current key
	Key string
}

type DictItemHolder struct {
	Key string
	Val interface{}
}

type ValueHolder struct {
	Val interface{}
}

// Add methods

func (c *ListHolder) Add(value interface{}) {
	c.List = append(c.List, value)
}

func (c *DictHolder) Add(value interface{}) {
	if c.Key == "" {
		c.Key = string(value.([]byte))
	} else {
		c.Dict[c.Key] = value
		// reset
		c.Key = ""
	}
}

func (c *ValueHolder) Add(value interface{}) {
	c.Val = value
}

// Obj methods

func (c *ListHolder) Obj() interface{} {
	return c.List
}

func (c *DictHolder) Obj() interface{} {
	return c.Dict
}

func (c *ValueHolder) Obj() interface{} {
	return c.Val
}

// Empty methods

func (c *ListHolder) Empty() bool {
	return c.List == nil
}

func (c *DictHolder) Empty() bool {
	return c.Dict == nil
}

func (c *ValueHolder) Empty() bool {
	return c.Val == nil
}

// now let's do the actual parsing

func parse2(container Holder, reader *bufio.Reader) Holder {
	b, err := reader.ReadByte()
	if err != nil {
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
		container.Add(val)
		return parse2(container, reader)
	case 'l':
		c := parse2(&ListHolder{List: make([]interface{}, 0)}, reader)
		if !container.Empty() {
			container.Add(c.(*ListHolder).List)
		} else {
			container = c
		}
		return parse2(container, reader)
	case 'd':
		c := parse2(&DictHolder{Dict: make(map[string]interface{})}, reader)
		if !container.Empty() {
			container.Add(c.(*DictHolder).Dict)
		} else {
			container = c
		}
		return parse2(container, reader)
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
		container.Add(val)
		return parse2(container, reader)
	}
	return container
}

func ParseBencoded2(r io.Reader) interface{} {
	reader := bufio.NewReader(r)

	// kick this off by passing an empty holder
	container := parse2(&ValueHolder{}, reader)
	return container.Obj()
}

func fillStruct(o interface{}, d map[string]interface{}) {
	var structure reflect.Type
	if reflect.TypeOf(o).Kind() != reflect.Struct {
		structure = reflect.TypeOf(o).Elem()
		fmt.Printf("non-structure is of type %s, %s\n", reflect.TypeOf(o), structure)
	} else {
		structure = reflect.TypeOf(o)

		fmt.Printf("structure is of type %s, %#v\n", structure, o)
	}
	//mutable := reflect.ValueOf(&structure)
	for i := 0; i < structure.NumField(); i++ {
		f := structure.Field(i)
		tag := f.Tag.Get("bencode")
		fmt.Printf("tag: %v, %s\n", tag, f.Name)
		if val, ok := d[tag]; ok {
			if f.Type.Kind() != reflect.Struct {
				fmt.Printf("not-struct %s\n", f.Tag)
				bindat := reflect.ValueOf(val).Convert(f.Type)
				reflect.ValueOf(o).Elem().Field(i).Set(bindat)
				if tag == "name" {
					fmt.Printf("name: %v\n%#v\n", bindat, o)
				}
			} else {
				oo := reflect.New(f.Type)
				//oo.Elem().Field(0).Set(reflect.ValueOf("abc"))
				fmt.Printf("%#v\n", oo)
				for k := range val.(map[string]interface{}) {
					fmt.Printf("\tk:%s\n", k)
				}
				fillStruct(oo.Interface(), val.(map[string]interface{}))
				fmt.Printf("after: %#v\n", oo)
				reflect.ValueOf(o).Elem().Field(i).Set(oo.Elem())
			}
		}
	}
}

func ParseTorrentFile2(r io.Reader) *BETorrent {
	obj := ParseBencoded2(r)
	d, ok := obj.(map[string]interface{})
	if !ok {
		panic("Unable to parse torrent")
	}
	for k, _ := range d {
		fmt.Printf("%s\n", k)
	}
	betorrent := &BETorrent{}
	fillStruct(betorrent, d)
	// st := reflect.TypeOf(betorrent)
	// mutable := reflect.ValueOf(&betorrent).Elem()
	// for i := 0; i < mutable.NumField(); i++ {
	// 	f := st.Field(i)
	// 	tag := f.Tag.Get("bencode")
	// 	if val, ok := d[tag]; ok {
	// 		if reflect.ValueOf(f).Kind() != reflect.Struct {
	// 			bindat := reflect.ValueOf(val).Convert(f.Type)
	// 			mutable.Field(i).Set(bindat)
	// 		}
	// 	}
	// }
	fmt.Printf("%#v", betorrent)
	return betorrent
}
