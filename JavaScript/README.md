Sereal implementation in JavaScript
===================================

Supports up to Sereal v3.0

Example can be run at example/index.html, please run it using a web server (http) and open the console to see the results

Usage
```
var dec = new Sereal.Decoder();
var doc = dec.decodeDocument(bytes);  // DataReader, or any data object that can be passed into DataReader (e.g. Uint8Array)
var data = doc.body;
console.log(data);
```
