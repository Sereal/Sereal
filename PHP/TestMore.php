<?php

function done_testing ()
{
	global $__TEST_COUNT;
	global $__TEST_FAILS;
	print "1..$__TEST_COUNT\n";
	exit($__TEST_FAILS);
}

function ok ($expr, $name=null)
{
	global $__TEST_FAILS;
	global $__TEST_COUNT; $__TEST_COUNT++;
	if ($name===null) $name = '???';
	
	if ($expr) {
		print "ok $__TEST_COUNT - $name\n";
	}
	else {
		$__TEST_FAILS++;
		print "not ok $__TEST_COUNT - $name\n";
	}
}

function is ($a, $b, $name=null)
{
	if ($name===null) $name = "is '$b'";
	$ok = ($a==$b);
	ok($ok, $name);
}

function like ($str, $re, $name=null)
{
	if ($name===null) $name = "like $re";
	$ok = preg_match($re, $str);
	ok($ok, $name);
}

function is_deeply ($a, $b, $name=null)
{
	if ($name===null) $name = 'is_deeply';
	ok(deep_array_compare($a, $b), $name);
}

function deep_array_compare ($a, $b)
{
	if (is_object($a)) $a = (array)$a;
	if (is_object($b)) $b = (array)$b;
	
	if (count($a) != count($b))
		return false;
	
	foreach ($a as $k => $av) {
		if (!array_key_exists($k, $b))
			return false;
		$bv = $b[$k];
		
		if (is_array($av) && is_array($bv)) {
			if (!deep_array_compare($av, $bv))
				return false;
		}
		elseif (is_array($av) || is_array($bv)) {
			return false;
		}
		elseif ($a != $b) {
			return false;
		}
	}
	
	return true;
}

