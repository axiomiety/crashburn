package data

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"reflect"
	"sort"
	"strconv"
)

// this is the interface that various containers implement

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
	} else {
		structure = reflect.TypeOf(o)
	}
	for i := 0; i < structure.NumField(); i++ {
		f := structure.Field(i)
		tag := f.Tag.Get("bencode")
		if val, ok := d[tag]; ok {
			if f.Type.Kind() != reflect.Struct {
				bindat := reflect.ValueOf(val).Convert(f.Type)
				reflect.ValueOf(o).Elem().Field(i).Set(bindat)
			} else {
				oo := reflect.New(f.Type)
				fillStruct(oo.Interface(), val.(map[string]interface{}))
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
	betorrent := &BETorrent{}
	fillStruct(betorrent, d)
	return betorrent
}

func Encode(buffer *bytes.Buffer, o interface{}) {
	value := reflect.ValueOf(o)
	switch value.Kind() {
	case reflect.Int, reflect.Int16, reflect.Int32, reflect.Int64:
		buffer.WriteByte('i')
		buffer.WriteString(strconv.Itoa(int(value.Int())))
		buffer.WriteByte('e')
	case reflect.Uint, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		buffer.WriteByte('i')
		buffer.WriteString(strconv.Itoa(int(value.Uint())))
		buffer.WriteByte('e')
	case reflect.String:
		buffer.WriteString(strconv.Itoa(len(value.Interface().(string))))
		buffer.WriteString(":")
		buffer.WriteString(value.Interface().(string))
	case reflect.Slice:
		buffer.WriteByte('l')
		// so this is a bit funky - we can't convert e.g. []int to []interface{}
		// directly
		temp := make([]interface{}, value.Len())

		for i := 0; i < value.Len(); i++ {
			temp[i] = value.Index(i).Interface()
		}
		for _, val := range temp {
			Encode(buffer, val)
		}
		buffer.WriteByte('e')
	case reflect.Map:
		buffer.WriteByte('d')
		temp := make(map[string]interface{}, value.Len())

		// we need a map[string]interface{}
		iter := value.MapRange()
		for iter.Next() {
			k := iter.Key()
			v := iter.Value()
			temp[k.Interface().(string)] = v.Interface()
		}

		// keys need to be sorted alphabetically
		keys := make([]string, 0, len(temp))
		for k := range temp {
			keys = append(keys, k)
		}
		sort.Strings(keys)

		for _, key := range keys {
			// first we write the key
			Encode(buffer, key)
			// then the value
			Encode(buffer, temp[key])
		}
		buffer.WriteByte('e')
	default:
		panic(fmt.Sprintf("can't handle type %s", value.Kind()))
	}

}
