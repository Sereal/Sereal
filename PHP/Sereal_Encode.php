<?php namespace Sereal;

define('Sereal\TAG_UNDEF',  "\x25");
define('Sereal\TAG_FALSE',  "\x3A");
define('Sereal\TAG_TRUE',   "\x3B");

function varint ($i) {
	$rv = "";
	do {
		$v = $i & 0x7F;
		$i = $i >> 7;
		if ($i != 0)
			$v |= 0x80;
		$rv .= chr($v);
	} while ($i != 0);
	return $rv;
}

class Encoder
{
	public function encode ($obj)
	{
		$sereal = '';
		$sereal .= $this->_make_header();
		$sereal .= $this->_encode($obj);
		return $sereal;
	}
	
	protected function _make_header ()
	{
		return "=srl\x01\x00";
	}
	
	protected function _encode ($x)
	{
		if (is_int($x) || is_long($x)) {
			return $this->_encode_integer($x);
		}
		elseif (is_bool($x)) {
			return $x ? TAG_TRUE : TAG_FALSE;
		}
		elseif (is_null($x)) {
			return TAG_UNDEF;
		}
		elseif (is_string($x)) {
			return $this->_encode_string($x);
		}
		elseif (is_float($x)) {
			return $this->_encode_float($x);
		}
		elseif (is_array($x) && count($x) && (array_keys($x) !== range(0, sizeof($x) - 1))) {
			return $this->_encode_hash($x);
		}
		elseif (is_array($x)) {
			return $this->_encode_array($x);
		}
	}
	
	protected function _encode_integer ($i)
	{
		if ($i >= 0 && $i < 16) {
			return chr($i);
		}
		elseif ($i < 0 && $i > -16) {
			return chr(32 + $i);
		}
		
		return varint($i);
	}
	
	protected function _encode_string ($bytes)
	{
		$sereal = "+" . varint(strlen($bytes));
		if (strlen($bytes) < 32)
			$sereal = chr(96 + strlen($bytes));
		
		$sereal .= $bytes;
		
		return $sereal;
	}
	
	protected function _encode_array ($arr)
	{
		$sereal = "+" . varint(count($arr));
		if (count($arr) < 16)
			$sereal = chr(64 + count($arr));
		
		for ($i = 0; $i < count($arr); $i++)
			$sereal .= $this->_encode($arr[$i]);
		
		return $sereal;
	}

	protected function _encode_hash ($arr)
	{
		$sereal = "*" . varint(count($arr));
		if (count($arr) < 16)
			$sereal = chr(80 + count($arr));
		
		foreach ($arr as $k => $v) {
			$sereal .= $this->_encode($k);
			$sereal .= $this->_encode($v);
		}
		
		return $sereal;
	}
	
	protected function _encode_float ($n)
	{
		die("TODO");
	}
}

$e = new Encoder;

print str_replace('%', '\\x', urlencode( $e->encode(array(1,2,array("foo"=>"bar","baz"=>"quux"))) ));
print "\n";