package sereal

import (
	"io/ioutil"
	"math/rand"
	"path/filepath"
	"testing"
	"time"
)

func BenchmarkMerger(b *testing.B) {
	files, _ := filepath.Glob("data/*.srl")
	if files == nil {
		b.Fatal("no files found")
	}

	var data [][]byte
	for _, file := range files {
		buf, ok := ioutil.ReadFile(file)
		if ok != nil {
			b.Fatal("failed to read file: " + file)
		}

		data = append(data, buf)
	}

	b.ResetTimer()

	m := NewMerger()
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	for i := 0; i < b.N; i++ {
		buf := data[r.Int()%len(data)]
		_, err := m.Append(buf)
		if err != nil {
			b.Fatal(err)
		}
	}
}
