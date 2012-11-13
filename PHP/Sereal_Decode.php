<?php namespace Sereal;

class ReservedWord  extends \Exception { };
class Unimplemented extends \Exception { };
class Malformed     extends \Exception { };
class UnexpectedEnd extends Malformed  { };

# probably pretty slow
function varint_get (&$str, $wantarray=false)
{
	$encoded = "";
	while ( ord($str) & 0x80 ) {
		$encoded .= substr($str, 0, 1);
		$str = substr($str, 1);
	}
	
	if (empty($str))
		throw new UnexpectedEnd("string unexpectedly ended parsing VARINT");
	
	# final digit
	$encoded .= substr($str, 0, 1);
	$str = substr($str, 1);
	
	$length = strlen($encoded);
	$number = '';
	while (strlen($encoded)) {
		$number  = sprintf('%07b', ord($encoded) & 0x7F) . $number;
		$encoded = substr($encoded, 1);
	}
	
	if ($wantarray)
		return array(bindec($number), $length);
	
	return bindec($number);
}

class Decoder
{
	protected $bytes;
	protected $offset;
	protected $offset_map;
	protected $header;
	
	static protected $_constants = array(
		'25' => null,
		'3A' => false,
		'3B' => true,
	);
	
	static protected $_track_replace = array(
		'8'  => '0',
		'9'  => '1',
		'A'  => '2',
		'B'  => '3',
		'C'  => '4',
		'D'  => '5',
		'E'  => '6',
		'F'  => '7',
	);
	
	public function decode ($bytes)
	{
		$this->offset     = 0;
		$this->bytes      = $bytes;
		$this->offset_map = array();
		$this->header     = "";
		
		$this->_decode_header();
		return $this->_decode();
	}
	
	protected function _decode_header ()
	{
		if ($this->_take(4) != "=srl") {
			throw new Malformed("no magic number");
		}
		
		if ($this->_take(1) != "\x01") {
			throw new Malformed("unsupported Sereal version");
		}
		
		$header_size = $this->_d_20();
		$this->header = $this->_take($header_size);
	}
	
	protected function _decode ()
	{
		$char = strtoupper( bin2hex( substr($this->bytes, 0, 1) ) );
		$this->bytes = substr($this->bytes, 1);
		$offset = $this->offset++;
		
		$track = false;
		if (preg_match('/^([89A-F])(.)$/', $char, $match)) {
			$track = true;
			$char  = self::$_track_replace[ $match[1] ] . $match[2];
		}
		
		# By God, isn't PHP code "beautiful"...
		$decoded = call_user_func( array($this, "_d_$char") );
		
		# Keep reference to the decoded object so that we can
		# refer to it later...
		if ($track)
			$this->offset_map[$offset] =& $decoded;
		
		return $decoded;
	}
	
	protected function _take ($n)
	{
		$string = substr($this->bytes, 0, $n);
		$this->bytes = substr($this->bytes, $n);
		$this->offset += $n;
		return $string;
	}
	
	protected function _take_array ($n)
	{
		$a = array();
		while ($n-->0) {
			$a[] = $this->_decode();
		}
		return $a;
	}

	protected function _take_hash ($n)
	{
		$a = array();
		while ($n-->0) {
			$k = $this->_decode();
			$v = $this->_decode();
			$a[$k] = $v;
		}
		return $a;
	}

	public function _d_20 ()  # VARINT
	{
		list($number, $length) = varint_get($this->bytes, true);
		$this->offset += $length;
		return $number;
	}
	
	public function _d_26 ()  # BINARY
	{
		$length = $this->_d_20();
		return $this->_take($length);
	}
	
	# It's not entirely clear how/if this should differ from
	# BINARY in PHP. PHP 5 has no mechanism for flagging a
	# string as UTF8; and PHP 6 is not forthcoming.
	#
	public function _d_27 ()  # STR_UTF8
	{
		$length = $this->_d_20();
		return $this->_take($length);
	}

	public function _d_28 ()  # REFN
	{
		$next =& $this->_decode();
		return $next;
	}

	public function _d_29 ()  # REFP
	{
		$offset = $this->_d_20();
		return $this->offset_map[$offset];
	}

	public function _d_2A ()  # HASH
	{
		$length = $this->_d_20();
		return $this->_take_hash($length);
	}

	public function _d_2B ()  # ARRAY
	{
		$length = $this->_d_20();
		return $this->_take_array($length);
	}

	public function __call ($name, $args)
	{
		if (preg_match('/^_d_0([A-F0-9])$/', $name, $match)) {
			return hexdec( $match[1] );
		}
		if (preg_match('/^_d_1([A-F0-9])$/', $name, $match)) {
			return hexdec( $match[1] ) - 16;
		}
		if (preg_match('/^_d_(3[2-9])$/', $name, $match)) {
			throw new ReservedWord("reserved tag ${match[1]} used");
		}
		if (preg_match('/^_d_(4[A-F0-9])$/', $name, $match)) {
			return $this->_take_array(hexdec( $match[1] ) - 64);
		}
		if (preg_match('/^_d_(5[A-F0-9])$/', $name, $match)) {
			return $this->_take_hash(hexdec( $match[1] ) - 80);
		}
		if (preg_match('/^_d_([67][A-F0-9])$/', $name, $match)) {
			return $this->_take(hexdec( $match[1] ) - 96);
		}
		if (preg_match('/^_d_([A-F0-9]{2})$/', $name, $match)) {
			if (array_key_exists($match[1], self::$_constants)) {
				return self::$_constants[ $match[1] ];
			}
		}
		
		throw new Unimplemented("unimplemented function $name");
	}
}

$d = new Decoder;

print_r( $d->decode("=srl\x01\x00Bb12\xE3123x") );
print_r( $d->decode("=srl\x01\x00(Qb12\xE3123x") );

