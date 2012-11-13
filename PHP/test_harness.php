<?php

require 'Sereal.php';
require 'TestMore.php';

function sereal_test_json ($json, $name=null)
{
	if ($name===null)
		$name = strlen($json) < 60 ? $json : substr($json, 0, 52) . "...";
	return sereal_test(json_decode($json), $name);
}

function sereal_test ($orig, $name=null)
{
	try {
		$got = sereal_decode(sereal_encode($orig));
	}
	catch (Exception $e) {
		$got = null;
	}
	return is_deeply($got, $orig, $name);
}

sereal_test_json('[1,2,3,4,5]');
sereal_test_json('["a", "bc", "def", "hijk", "lmnop", "qrstuv"]');
sereal_test_json(file_get_contents('corpus.json'), 'corpus.json');

done_testing();
