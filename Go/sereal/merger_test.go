package sereal

import (
	"bytes"
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

func TestMergerSizesV6Tags(t *testing.T) {
	m := NewMergerV3()
	for _, doc := range []string{"3df3726c06003e260676312e322e33", "3df3726c06003ce807"} {
		if _, err := m.Append(mustHex(t, doc)); err != nil {
			t.Fatalf("Append(%s) error: %v", doc, err)
		}
	}

	b, err := m.Finish()
	if err != nil {
		t.Fatal(err)
	}

	vstring := bytes.Index(b, mustHex(t, "3e260676312e322e33"))
	negVarint := bytes.Index(b, mustHex(t, "3ce807"))
	if vstring < 0 || negVarint < vstring {
		t.Errorf("merged document %x lacks the v-string followed by NEG_VARINT", b)
	}
}
