package com.booking.sereal.impl;

import java.util.Arrays;

public class RefpMap {
	public static final Object NOT_FOUND = new Object();

	private static final long NO_KEY = -1;
	private long[] keys;
	private Object[] values;
	private int size, maxLoad, modulus;

	public RefpMap() {
		init(32);
	}

	private void init(int capacity) {
		keys = new long[capacity];
		values = new Object[capacity];
		Arrays.fill(keys, NO_KEY);
		modulus = capacity - 1;
		maxLoad = (int) (capacity * 0.80);
		size = 0;
	}

	public void clear() {
		Arrays.fill(keys, NO_KEY);
		size = 0;
	}

	public final Object get(long key) {
		int slot = findSlot(key);

		return keys[slot] == NO_KEY ? NOT_FOUND : values[slot];
	}

	public final void put(long key, Object value) {
		int slot = findSlot(key);

		if (keys[slot] != key) {
			if (size == maxLoad) {
				rehash();
				slot = findSlot(key);
			}

			keys[slot] = key;
			values[slot] = value;
			size++;
		} else
			values[slot] = value;
	}

	private void rehash() {
		long[] oldKeys = keys;
		Object[] oldValues = values;

		init(keys.length * 2);

		for (int i = 0, max = oldKeys.length; i < max; ++i)
			put(oldKeys[i], oldValues[i]);
	}

	private int findSlot(long key) {
		int slot = hash6432Shift(key) & modulus;
		while (keys[slot] != NO_KEY && keys[slot] != key)
			slot = (slot + 1) & modulus;
		return slot;
	}

	// http://burtleburtle.net/bob/hash/integer.html
	// not tested for the actual distribution of offsets in Sereal
	private int hash6432Shift(long key) {
		key = (~key) + (key << 18); // key = (key << 18) - key - 1;
		key = key ^ (key >>> 31);
		key = key * 21; // key = (key + (key << 2)) + (key << 4);
		key = key ^ (key >>> 11);
		key = key + (key << 6);
		key = key ^ (key >>> 22);
		return (int) key;
	}
}
