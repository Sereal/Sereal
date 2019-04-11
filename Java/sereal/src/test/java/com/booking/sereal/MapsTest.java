package com.booking.sereal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.booking.sereal.impl.BytearrayCopyMap;
import com.booking.sereal.impl.IdentityMap;
import com.booking.sereal.impl.StringCopyMap;

import org.junit.Test;

public class MapsTest {
	@Test
	public void identity() {
		IdentityMap map = new IdentityMap();
		Object key1 = new Object(), key2 = new Object(), key3 = new Object();

		assertEquals(map.get(key1), IdentityMap.NOT_FOUND);
		assertEquals(map.get(key2), IdentityMap.NOT_FOUND);

		map.put(key1, 1);
		map.put(key2, 2);

		assertEquals(map.get(key1), 1);
		assertEquals(map.get(key2), 2);
		assertEquals(map.get(key3), IdentityMap.NOT_FOUND);

		map.put(key2, 3);

		assertEquals(map.get(key2), 3);
	}

	@Test
	public void bytearrayCopy() {
		BytearrayCopyMap map = new BytearrayCopyMap();

		assertEquals(map.get(new byte[] {}), BytearrayCopyMap.NOT_FOUND);
		assertEquals(map.get(new byte[] { 0x01 }), BytearrayCopyMap.NOT_FOUND);

		map.put(new byte[] {}, 1);
		map.put(new byte[] { 0x02 }, 3);

		assertEquals(map.get(new byte[] {}), 1);
		assertEquals(map.get(new byte[] { 0x01 }), BytearrayCopyMap.NOT_FOUND);
		assertEquals(map.get(new byte[] { 0x02 }), 3);

		map.put(new byte[] { 0x02 }, 4);

		assertEquals(map.get(new byte[] { 0x02 }), 4);
	}

	@Test
	public void stringCopy() {
		StringCopyMap map = new StringCopyMap();

		assertTrue(new String("a") != new String("a"));

		assertEquals(map.get(new String()), BytearrayCopyMap.NOT_FOUND);
		assertEquals(map.get(new String("b")), BytearrayCopyMap.NOT_FOUND);

		map.put(new String(), 1);
		map.put(new String("b"), 3);

		assertEquals(map.get(new String()), 1);
		assertEquals(map.get(new String("a")), BytearrayCopyMap.NOT_FOUND);
		assertEquals(map.get(new String("b")), 3);

		map.put(new String("b"), 4);

		assertEquals(map.get(new String("b")), 4);
	}
}
